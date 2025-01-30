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
    DOT,    // Always match current character
    RETRY,
    JUMPTABLE,
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
    fn bool isValidStartingChar(char c);
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

fn bool MatchAny.isValidStartingChar(&self, char c) @dynamic {
    return self.expression.isValidStartingChar(c);
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

fn bool MatchMany.isValidStartingChar(&self, char c) @dynamic {
    return self.expression.isValidStartingChar(c);
}

struct MatchString (Grammar) {
    String string;
}

fn void MatchString.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = STRING, .argument = (uptr)self.string.ptr, .optArgument = self.string.len, .ok = ok, .ng = ng });
}

fn bool MatchString.isValidStartingChar(&self, char c) @dynamic {
    return self.string.len > 0 && self.string[0] == c;
}

struct Sequence (Grammar) {
    Grammar[] expressions;
}

fn void Sequence.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = DROP, .ok = ok, .ng = ok });
    usz nextOk = instructions.len() - 1;
    (*instructions).push(Instruction { .type = POP, .ok = ng, .ng = ng });
    usz localNg = instructions.len() - 1;

    foreach_r (expression : self.expressions) {
        expression.emitByteCode(instructions, nextOk, localNg);
        nextOk = instructions.len() - 1;
    }

    (*instructions).push(Instruction { .type = PUSH, .ok = nextOk, .ng = nextOk });
}

fn bool Sequence.isValidStartingChar(&self, char c) @dynamic {
    return self.expressions.len > 0 && self.expressions[0].isValidStartingChar(c);
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

fn bool ReturnSequence.isValidStartingChar(&self, char c) @dynamic {
    return self.expressions.len > 0 && self.expressions[0].isValidStartingChar(c);
}

struct Alternation (Grammar) {
    Grammar[] expressions;
}

fn void Alternation.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = DROP, .ok = ok, .ng = ok });
    usz localOk = instructions.len() - 1;
    (*instructions).push(Instruction { .type = POP, .ok = ng, .ng = ng });
    usz nextNg = instructions.len() - 1;

    foreach_r (expression : self.expressions) {
        expression.emitByteCode(instructions, localOk, nextNg);
        nextNg = instructions.len() - 1;
    }

    (*instructions).push(Instruction { .type = PUSH, .ok = nextNg, .ng = nextNg });
}

fn bool Alternation.isValidStartingChar(&self, char c) @dynamic {
    if (self.expressions.len == 0) return false;
    foreach (expr : self.expressions) {
        if (expr.isValidStartingChar(c)) return true;
    }
    return false;
}

struct LexerRoot (Grammar) {
    Grammar[] expressions;
}

fn void LexerRoot.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    usz nextNg = ng;
    usz[] jumpTable = allocator::alloc_array(allocator::heap(), usz, 256);
    foreach_r (expression : self.expressions) {
        expression.emitByteCode(instructions, ok, nextNg);
        nextNg = instructions.len() - 1;
        for (int c = 0; c < 256; c++) {
            if (expression.isValidStartingChar((char)c)) {
                jumpTable[c] = nextNg;
            }
        }
    }
    (*instructions).push(Instruction { .type = JUMPTABLE, .argument = (uptr)jumpTable.ptr, .ok = nextNg, .ng = nextNg });
}

fn bool LexerRoot.isValidStartingChar(&self, char c) @dynamic {
    unreachable();
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

fn bool FollowedBy.isValidStartingChar(&self, char c) @dynamic {
    unreachable();
}

distinct Dot = char;

const Grammar DOT = (Grammar)&&(Dot)'0';

fn void Dot.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    (*instructions).push(Instruction { .type = DOT, .ok = ok, .ng = ok });
}

fn bool Dot.isValidStartingChar(&self, char c) @dynamic {
    return true;
}

struct Context {
    usz position;
    usz beginTokenIndex;
    int tokenType;
}

def ContextList = List(<Context>);

fn void runVm(InstructionList instructions, String input, usz inputLength, TokenList* tokens) {
    ContextList stack;
    stack.temp_init();

    Context ctx;

    Instruction inst = instructions[0];
    for LOOP: (;;) {
        switch (inst.type) {
            case PUSH:
                stack.push(ctx);
                inst = instructions[inst.ok];
            case POP:
                ctx = stack.pop()!!;
                inst = instructions[inst.ok];
            case DROP:
                stack.pop()!!;
                inst = instructions[inst.ok];
            case CLASS:
                CharClass* class = (CharClass*)inst.argument;
                if (ctx.position < inputLength && class.contains(input[ctx.position])) {
                    ctx.position++;
                    inst = instructions[inst.ok];
                } else {
                    inst = instructions[inst.ng];
                }
            case STRING:
                String s = (String)((char*)inst.argument)[:inst.optArgument];
                if (s == input[ctx.position:inst.optArgument]) {
                    ctx.position++;
                    inst = instructions[inst.ok];
                } else {
                    inst = instructions[inst.ng];
                }
            case BEGIN:
                ctx.beginTokenIndex = ctx.position;
                ctx.tokenType = (int)inst.argument;
                inst = instructions[inst.ok];
            case END:
                // TODO: move token type to END instruction argument
                tokens.push(Token {
                    (LexingState)ctx.tokenType,
                    input[ctx.beginTokenIndex..ctx.position - 1],
                });
                inst = instructions[inst.ok];
            case CHAR:
                unreachable();
            case DOT:
                ctx.position++;
                inst = instructions[inst.ok];
            case RETRY:
                // io::printfn("%d / %d", ctx.position, inputLength);
                if (ctx.position >= inputLength) {
                    break LOOP;
                }
                inst = instructions[0];
            case JUMPTABLE:
                usz[] jumpTable = ((ulong*)inst.argument)[:256];
                inst = instructions[jumpTable[input[ctx.position]]];
        }
    }
}

fn TokenList testVm(String input, usz inputLength) {

    Clock clock = clock::now();
    io::printn("\n---VM TEST---");

    // g = <4269> [a-z]* &"asdf"
    //   | <1337> "bonsoir"

    // Grammar g = &&Alternation {
    //     .expressions = {
    //         &&ReturnSequence {
    //             .tokenType = 4269,
    //             .expressions = {
    //                 &&MatchMany {
    //                     .expression = charclass::newFromString("a-z", allocator::temp())
    //                 },
    //                 &&FollowedBy {
    //                     .expression = &&MatchString {
    //                         .string = "asdf"
    //                     }
    //                 },
    //             },
    //         },
    //         &&ReturnSequence {
    //             .tokenType = 1337,
    //             .expressions = {
    //                 &&MatchString {
    //                     .string = "bonsoir"
    //                 },
    //             },
    //         },
    //     }
    // };

    // g = [ \n\t]*
    //   | <2> [A-Za-z_][0-9A-Za-z_]
    //   | <3> [0-9][0-9A-Za-z_]
    //   | <1> .
    CharClass* identHead = charclass::newFromString("A-Za-z_");
    CharClass* identBody = charclass::newFromString("0-9A-Za-z_");
    CharClass* numberHead = charclass::newFromString("0-9");
    CharClass* numberBody = identBody;
    CharClass* whitespace = charclass::newFromString(" \r\n\t");

    Grammar g = &&LexerRoot {
        .expressions = {
            &&Sequence {
                .expressions = {
                    &&MatchMany {
                        .expression = whitespace
                    },
                }
            },
            &&ReturnSequence {
                .tokenType = 2,
                .expressions = {
                    identHead,
                    &&MatchAny {
                        .expression = identBody
                    },
                },
            },
            &&ReturnSequence {
                .tokenType = 3,
                .expressions = {
                    numberHead,
                    &&MatchAny {
                        .expression = numberBody
                    },
                },
            },
            &&ReturnSequence {
                .tokenType = 1,
                .expressions = {
                    DOT
                }
            },
        }
    };

    InstructionList instructions;
    instructions.temp_init();

    instructions.push(Instruction { .type = RETRY, .ok = 0, .ng = 0 });

    g.emitByteCode(&instructions, 0, 0);

    io::printn();
    instructions.reverse();
    foreach (index, &inst : instructions) {
        inst.ok = instructions.len() - 1 - inst.ok;
        inst.ng = instructions.len() - 1 - inst.ng;
        if (inst.type == JUMPTABLE) {
            usz[] jumpTable = ((ulong*)inst.argument)[:256];
            foreach (&addr : jumpTable) {
                *addr = instructions.len() - 1 - *addr;
            }
        }
        // io::printf("%03d: ", index);
        // io::printn(*inst);
    }

    TokenList tokens;
    tokens.new_init();

    runVm(instructions, input, inputLength, &tokens);

    NanoDuration duration = clock.mark();

    io::printfn("Input size : %d Bytes", inputLength);
    io::printfn("Lexing finished in %s", duration);

    io::printfn("Recognized %d tokens", tokens.len());
    io::printfn("Speed : %.02f Million tokens per second", tokens.len()/duration.to_sec()/1_000_000);

    return tokens;
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
    usz inputLength = 0;

    while (try size = inputFile.read(&buffer) && size > 0) {
        input[inputLength:size] = buffer[:size];
        inputLength += size;
    }

    usz tokenBegin = 0;
    TokenList tokens;
    tokens.new_init();
    LexingState state = WHITESPACE;

    CharClass* identHead = charclass::newFromString("A-Za-z_");
    CharClass* identBody = charclass::newFromString("0-9A-Za-z_");
    CharClass* numberHead = charclass::newFromString("0-9");
    CharClass* numberBody = identBody;

    foreach (usz cursor, char c : input[:inputLength]) {
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
    io::printfn("Input size : %d Bytes", inputLength);
    io::printfn("Lexing finished in %s", duration);

    io::printfn("Recognized %d tokens", tokens.len());
    io::printfn("Speed : %.02f Million tokens per second", tokens.len()/duration.to_sec()/1_000_000);


    TokenList vmTokens = testVm(input, inputLength);

    int puncts;
    int idents;
    int nums;
    foreach (i, token : tokens) {
        assert (token.type == vmTokens[i].type);
        assert (token.content == vmTokens[i].content);
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

