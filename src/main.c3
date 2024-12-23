import std::io;
import std::time;
import std::collections;

// Parsing lexer grammar file

// enum States {
//     OUTER,     // Expect Keyword, then { --> BLOCK(Keyword) OR EOF --> DONE
//     BLOCK,     // Expect Statement --> STATEMENT OR } --> OUTER
//     STATEMENT, // Depends on the keyword --> BLOCK
//     DONE,      // Good Job
// }

fn bool char.isWhitespace(char c) => (c == ' ' || c == '\t' || c == '\n' || c == '\r');

fault ParsingError {
    INVALID_ESCAPE_SEQUENCE,
}

fn char! char.unescape(char c) {
    switch (c) {
        case 'a': return '\a';
        case 'b': return '\b';
        case 'e': return '\e';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return '\v';
        case '0': return '\0';
        case '\\': return '\\';
        default: return ParsingError.INVALID_ESCAPE_SEQUENCE?;
    }
}

def CharClass = char[32];

fn bool CharClass.contains(self, char c) => ((self[c / 8] >> (c % 8)) & 1 == 1);

fn CharClass* newFromString(String s, Allocator allocator = allocator::heap()) {
    CharClass* result = allocator::alloc_array(allocator, char, 32);
    mem::set(result, 0, 32);
    char lastChar;
    bool inRange = false;
    bool escaping = false;
    foreach (index, c : s) {
        if (c == '\\' && !escaping) {
            escaping = true;
            continue;
        }
        if (escaping) {
            c = c.unescape() ?? {|
                io::printfn("Encountered invalid escape sequence : \\%c", c);
                return ParsingError.INVALID_ESCAPE_SEQUENCE?;
            |}!!;
            escaping = false;
        }
        if (c == '-' && index != 0 && index != s.len - 1) {
            inRange = true;
        } else if (inRange) {
            for (char x = lastChar; x <= c; x++) {
                (*result)[x / 8] |= 1 << (x % 8);
            }
            inRange = false;
        } else {
            (*result)[c / 8] |= 1 << (c % 8);
            lastChar = c;
        }
    }
    return result;
}

fn void testCharClass() {
    CharClass* class = newFromString("a-z");
    defer free(class);
    assert(class.contains('a'));
    assert(!class.contains('0'));

    CharClass* class2 = newFromString("-01-9abc-ef");
    defer free(class2);
    assert(class2.contains('-'));
    assert(class2.contains('0'));
    assert(class2.contains('1'));
    assert(class2.contains('8'));
    assert(class2.contains('9'));
    assert(class2.contains('a'));
    assert(class2.contains('b'));
    assert(class2.contains('c'));
    assert(class2.contains('d'));
    assert(class2.contains('e'));
    assert(class2.contains('f'));
    assert(!class2.contains('g'));
    assert(!class2.contains('_'));

    CharClass* class3 = newFromString("01-9abc-ef-");
    defer free(class3);
    assert(class3.contains('-'));
    assert(class3.contains('0'));
    assert(class3.contains('1'));
    assert(class3.contains('8'));
    assert(class3.contains('9'));
    assert(class3.contains('a'));
    assert(class3.contains('b'));
    assert(class3.contains('c'));
    assert(class3.contains('d'));
    assert(class3.contains('e'));
    assert(class3.contains('f'));
    assert(!class3.contains('g'));
    assert(!class3.contains('_'));

    CharClass* class4 = newFromString("\\0-\\n");
    defer free(class4);
    assert(class4.contains('\0'));
    assert(class4.contains('\n'));
    assert(class4.contains('\x01'));
    assert(class4.contains('\x02'));
    assert(class4.contains('\x0a'));
    assert(!class4.contains('\x0b'));
    assert(!class4.contains('n'));
    assert(!class4.contains('0'));
}

// Lexing

enum LexingState {
    WHITESPACE,
    PUNCTUATION,
    IDENT,
    NUMERIC,
}

struct Token {
    LexingState type;
    String content;
    // usz begin;
    // usz length;
}

def TokenList = List(<Token>);

// const INPUT_FILE = "resources/input.c3";
const INPUT_FILE = "resources/sqlite3.c";
const BUFFER_SIZE = 65536;

fn void main() {
    testCharClass();
    // File grammarFile = file::open("yap.gmr", "r")!!;
    // defer (void)grammarFile.close();
    // File* stream = &grammarFile;

    Clock clock = clock::now();

    usz inputSize = file::get_size(INPUT_FILE)!!;
    File inputFile = file::open(INPUT_FILE, "r")!!;
    defer (void)inputFile.close();

    String input = (String)mem::alloc_array(char, inputSize);
    defer free(input);

    char[BUFFER_SIZE] buffer;
    usz totalSize = 0;

    while (try size = inputFile.read(&buffer) && size > 0) {
        input[totalSize:size] = buffer[:size];
        totalSize += size;
    }

    usz tokenBegin = 0;
    TokenList tokens;
    tokens.new_init();
    LexingState state = WHITESPACE;

    CharClass* identHead = newFromString("A-Za-z_");
    CharClass* identBody = newFromString("0-9A-Za-z_");
    CharClass* numberHead = newFromString("0-9");
    CharClass* numberBody = identBody;

    foreach (usz cursor, char c : input[:totalSize]) {
        switch (state) {
            case WHITESPACE:
            case PUNCTUATION:
                if (!c.isWhitespace()) {
                    switch {
                        case identHead.contains(c):
                            state = IDENT;
                            tokenBegin = cursor;
                        case numberHead.contains(c):
                            state = NUMERIC;
                            tokenBegin = cursor;
                        default:
                            state = PUNCTUATION;
                            tokens.push(Token { state, input[cursor..cursor] });
                    }
                }
            case IDENT:
                switch {
                    case c.isWhitespace():
                        tokens.push(Token { state, input[tokenBegin..cursor - 1] });
                        state = WHITESPACE;
                    case !identBody.contains(c):
                        tokens.push(Token { state, input[tokenBegin..cursor - 1] });
                        state = PUNCTUATION;
                        tokens.push(Token { state, input[cursor..cursor] });
                }
            case NUMERIC:
                switch {
                    case c.isWhitespace():
                        tokens.push(Token { state, input[tokenBegin..cursor - 1] });
                        state = WHITESPACE;
                    case !numberBody.contains(c):
                        tokens.push(Token { state, input[tokenBegin..cursor - 1] });
                        state = PUNCTUATION;
                        tokens.push(Token { state, input[cursor..cursor] });
                }
        }
    }

    NanoDuration duration = clock.mark();

    // foreach (token : tokens) {
    //     io::printn(token);
    // }
    io::printfn("Input size : %d Bytes", totalSize);
    io::printfn("Lexing finished in %s", duration);

    io::printfn("Recognized %d tokens", tokens.len());
    io::printfn("Speed : %.02f Million tokens per second", tokens.len()/duration.to_sec()/1_000_000);

    int puncts;
    int idents;
    int nums;
    foreach (token : tokens) {
        switch (token.type) {
            case PUNCTUATION:
                puncts++;
            case IDENT:
                idents++;
            case NUMERIC:
                nums++;
            default: ;
        }
    }

    io::printfn("Puncts : %d, Idents: %d, Nums: %d", puncts, idents, nums);


    // while (try line = io::treadline(stream)) {
    //     String trimmedLine = line.trim();
    //     io::printfn("%03d | %s", trimmedLine.len, trimmedLine);
    // }
}
