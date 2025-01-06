module parsing;

import std::io;
import std::time;
import std::collections;

import parsing::charclass;

fault ParsingError {
    INVALID_ESCAPE_SEQUENCE,
}

fn bool char.isWhitespace(char c) => (c == ' ' || c == '\t' || c == '\n' || c == '\r');

fn char! char.unescape(char c) {
    switch (c) {
        case  'a': return '\a';
        case  'b': return '\b';
        case  'e': return '\e';
        case  'f': return '\f';
        case  'n': return '\n';
        case  'r': return '\r';
        case  't': return '\t';
        case  'v': return '\v';
        case  '0': return '\0';
        case '\\': return '\\';
        default: return ParsingError.INVALID_ESCAPE_SEQUENCE?;
    }
}

enum InstructionType {
    PUSH,   // Save last token start and cursor position
    POP,    // Restore last save
    DROP,   // Forget last save
    CLASS,  // Test character class match
    CHAR,   // Test char equality
    STRING, // Test string equality (tester la perf)
    BEGIN,  // Begin Token (mark beginning of token)
    END,    // End Token   (push to token list)
}

struct Instruction {
    InstructionType type;
    uptr argument;
    uptr optArgument;
    usz ok;
    usz ng;
}

fn bool Instruction.equals(self, Instruction other) {
    return (
        self.type == other.type
        && self.argument == other.argument
        && self.ok == other.ok
        && self.ng == other.ng
    );
}

def InstructionList = List(<Instruction>);

interface Grammar {
    fn void emitByteCode(InstructionList* instructions, usz ok, usz ng);
}

// Regex Star
struct MatchAny (Grammar) {
    Grammar expression;
}

fn void MatchAny.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    usz lastInstructionIndex = instructions.len();
    self.expression.emitByteCode(instructions, ok, ok);
    (*instructions)[lastInstructionIndex].ok = instructions.len() - 1;
}

// Regex Plus
struct MatchMany (Grammar) {
    Grammar expression;
}

fn void MatchMany.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    usz lastInstructionIndex = instructions.len();
    self.expression.emitByteCode(instructions, ok, ok);
    (*instructions)[lastInstructionIndex].ok = instructions.len() - 1;
    self.expression.emitByteCode(instructions, instructions.len() - 1, ng);
}

struct MatchString (Grammar) {
    String string;
}

fn void MatchString.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = STRING, .argument = (uptr)self.string.ptr, .optArgument = self.string.len, .ok = ok, .ng = ng });
}

struct ReturnSequence (Grammar) {
    int tokenType;
    Grammar[] expressions;
}

// TODO: Optimize bytecode generation in case there is only one element in sequence or in alternation.
// Low priority as it should never happen in a real grammar anyway
fn void ReturnSequence.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = DROP, .ok = ok, .ng = ok });
    usz nextOk = instructions.len() - 1;
    (*instructions).push(Instruction { .type = POP, .ok = ng, .ng = ng });
    usz localNg = instructions.len() - 1;

    (*instructions).push(Instruction { .type = END, .ok = nextOk, .ng = nextOk });
    nextOk = instructions.len() - 1;

    foreach_r (expression : self.expressions) {
        expression.emitByteCode(instructions, nextOk, localNg);
        nextOk = instructions.len() - 1;
    }

    (*instructions).push(Instruction { .type = BEGIN, .argument = self.tokenType, .ok = nextOk, .ng = nextOk });
    nextOk = instructions.len() - 1;

    (*instructions).push(Instruction { .type = PUSH, .ok = nextOk, .ng = nextOk });
}

struct Alternation (Grammar) {
    Grammar[] expressions;
}

fn void Alternation.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = DROP, .ok = ok, .ng = ok });
    usz localOk = instructions.len() - 1;
    (*instructions).push(Instruction { .type = POP, .ok = ng, .ng = ng });
    usz localNg = instructions.len() - 1;

    usz nextNg = localNg;

    foreach_r (expression : self.expressions) {
        expression.emitByteCode(instructions, localOk, nextNg);
        nextNg = instructions.len() - 1;
    }

    (*instructions).push(Instruction { .type = PUSH, .ok = nextNg, .ng = nextNg });
}

struct FollowedBy (Grammar) {
    Grammar expression;
}

fn void FollowedBy.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = POP, .ok = ok, .ng = ok });
    usz localOk = instructions.len() - 1;
    (*instructions).push(Instruction { .type = POP, .ok = ng, .ng = ng });
    usz localNg = instructions.len() - 1;

    self.expression.emitByteCode(instructions, localOk, localNg);
    localOk = instructions.len() - 1;

    (*instructions).push(Instruction { .type = PUSH, .ok = localOk, .ng = localOk });
}

fn void testByteCodeGen() @test {
    // g = <4269> [a-z]* &"asdf"
    //   | <1337> "bonsoir"
    Grammar g =
        &&Alternation {
            .expressions = {
                &&ReturnSequence {
                    .tokenType = 4269,
                    .expressions = {
                        &&MatchMany {
                            .expression = charclass::newFromString("a-z", allocator::temp())
                        },
                        &&FollowedBy {
                            .expression =
                                &&MatchString {
                                    .string = "asdf"
                                }
                            }
                        },
                    }
                &&ReturnSequence {
                    .tokenType = 1337,
                    .expressions = {
                        &&MatchString {
                            .string = "bonsoir"
                        },
                    },
                },
            }
        };

    InstructionList instructions;
    instructions.temp_init();

    g.emitByteCode(&instructions, 0, 0);

    io::printn();
    instructions.reverse();
    foreach (index, &inst : instructions) {
        inst.ok = instructions.len() - 1 - inst.ok;
        inst.ng = instructions.len() - 1 - inst.ng;
        io::printf("%03d: ", index);
        io::printn(*inst);
    }
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
}

def TokenList = List(<Token>);

// const INPUT_FILE = "resources/input.c3";
const INPUT_FILE = "resources/sqlite3.c";
const BUFFER_SIZE = 65536;

fn void main() {
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

    CharClass* identHead = charclass::newFromString("A-Za-z_");
    CharClass* identBody = charclass::newFromString("0-9A-Za-z_");
    CharClass* numberHead = charclass::newFromString("0-9");
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

// Old Ideas
// Parsing lexer grammar file

// enum States {
//     OUTER,     // Expect Keyword, then { --> BLOCK(Keyword) OR EOF --> DONE
//     BLOCK,     // Expect Statement --> STATEMENT OR } --> OUTER
//     STATEMENT, // Depends on the keyword --> BLOCK
//     DONE,      // Good Job
// }

