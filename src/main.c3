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
    PUSH,      // Save last token start and cursor position
    POP,       // Restore last save
    DROP,      // Forget last save
    CLASS,     // Test character class match
    DOT,       // Always match current character
    RETRY,     // Retry while input has not been fully consumed
    JUMPTABLE, // Jumptable mechanism for fast lexing
    STRING,    // Test string equality (tester la perf)
    BEGIN,     // Begin Token (mark beginning of token)
    END,       // End Token   (push to token list)
    MOVPOS,    // Move value of current position to a register
    SEEK,      // Seek to position contained in register
    GREATER,   // Check if value in first register is greater than value in second register
    DELTOKEN,  // Delete token at given index counting from the end of the token list. (0 is last, 1 is second to last, etc...)
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
struct Star (Grammar) {
    Grammar expression;
}

fn void Star.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    usz lastInstructionIndex = instructions.len();
    self.expression.emitByteCode(instructions, ok, ok);
    (*instructions)[lastInstructionIndex].ok = instructions.len() - 1;
}

fn bool Star.isValidStartingChar(&self, char c) @dynamic {
    return self.expression.isValidStartingChar(c);
}

// Regex Plus
struct Plus (Grammar) {
    Grammar expression;
}

fn void Plus.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    usz lastInstructionIndex = instructions.len();
    self.expression.emitByteCode(instructions, ok, ok);
    (*instructions)[lastInstructionIndex].ok = instructions.len() - 1;
    self.expression.emitByteCode(instructions, instructions.len() - 1, ng);
}

fn bool Plus.isValidStartingChar(&self, char c) @dynamic {
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

// FIXME: If a sequence starts by a character that can match nothing, it needs to check the next element.
// Example :
//      seq = [_]* [a-z]
// Valid starting chars for this are _ and a-z

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

<*
    AST node for the lexer part of a Grammar
    - `Grammar[] keywords`      : the keywords of the language. Should be an array of pointers to `MatchString` structures
    - `Grammar[] wordsPatterns` : patterns that will recognize all "words" of the language, like identifiers or numbers
*>
struct Lexer (Grammar) {
    Grammar[] keywords;
    Grammar[] wordPatterns;
}

/*
    Structure du bytecode Lexer

    Skip whitespace (Plus [ \n\r\t], ok = RETRY, ng = next)
    Skip comments
    Strings
    Reconnaissance de la ponctuation
    Reconnaissance des mots + keywords
*/

// TODO: splitter les keywords par caractère de début.
// on rencontre le token "foreuse" -> jmptable 'f' -> "foreach" NG -> "for" NG -> plus de kw en 'f' -> fin de recherche de kw
// autrement dit, le .ng de la dernière instruction du dernier kw de chaque lettre doit pointer vers l'instruction qui suit le bytecode de reconnaissance des keywords

/*
; Assembleur bytecode parsing VM
; Besoin de checker si le pattern matche le keyword.
; Si oui, il faut s'en souvenir comme dernier endroit où jumper si le match du keyword est infructueux.
; Il faut aussi réfléchir à un moyen de vérifier que le kw ne fasse pas partie d'un mot plus grand
; Ex : `foreuse` ne doit pas matcher le kw `for`

; Idée d'algo

; Précondition : les words contiennent une dernière branche `| <0> .` pour toujours matcher au moins un caractère, qui renvoie un token invalide (id 0)
;   Note : le parser traitera les tokens invalides comme des erreurs et pourra déclencher son mécanisme de rattrapage d'erreur si besoin

; Essai de parsing de mots génériques puis de mots clés
MOVPOS r0
JUMPTABLE sur les words
MOVPOS r1
SEEK r0
JUMPTABLE sur les keywords
MOVPOS r2

; Pas de keyword
IF pas de kw trouvé = IF ¬(r2 > r1)
    SEEK r1, ok = RETRY

; Décision de si on garde un token de keyword ou de word
IF r1 > r2
    DEL dernier token
    SEEK r1, ok = RETRY
ELSE
    DEL avant-dernier token
    SEEK r2, ok = RETRY
*/

<*
    This assumes that keywords and punctuations are sorted in the correct order
*>
fn void Lexer.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {

    usz nextNg = ng;
    usz[] wordJumpTable = allocator::alloc_array(allocator::heap(), usz, 256);
    usz[] keywordJumpTable = allocator::alloc_array(allocator::heap(), usz, 256);

    instructions.push(Instruction { .type = RETRY, .ok = 0, .ng = 0 });
    usz retryAddr = nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = SEEK, .argument = 2, .ok = retryAddr, .ng = retryAddr });
    nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = DELTOKEN, .argument = 1, .ok = nextNg, .ng = nextNg });
    usz discardWordTokenAddr = nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = SEEK, .argument = 1, .ok = retryAddr, .ng = retryAddr });
    usz noKeywordParsedAddr = nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = DELTOKEN, .argument = 0, .ok = nextNg, .ng =  nextNg });
    nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = GREATER, .argument = 2, .optArgument = 0, .ok = nextNg, .ng = noKeywordParsedAddr });
    nextNg = instructions.len() - 1;

    instructions.push(Instruction { .type = GREATER, .argument = 1, .optArgument = 2, .ok = nextNg, .ng = discardWordTokenAddr });
    nextNg = instructions.len() - 1;

    /*
        Position avant opération dans r0
        Words dans r1
        keywords dans r2
    */

    (*instructions).push(Instruction { .type = MOVPOS, .argument = 2, .ok = nextNg, .ng = nextNg });
    nextNg = instructions.len() - 1;
    usz keywordNg = nextNg; /* const */
    usz keywordOk = keywordNg;

    // Besoin de set l'adresse par défaut de la jumpTable à l'adresse du handling du résultat du lexing des keywords
    for (int i = 0; i < 256; i++) {
        keywordJumpTable[i] = keywordNg;
    }

    // Keyword bytecode gen

    // Pour chaque nouveau caractère de début, on met à jour la JT,
    // et on set le NG des prochaines instructions comme étant celui d'avant la génération des instructions de keywords,
    // ce qui permet de skip les instructions dont on sait qu'elles échoueront forcément.

    int prevStartingChar = -1;

    // Keyword bytecode gen
    foreach_r (index, expression : self.keywords) {
        (*instructions).push(Instruction { .type = END, .ok = keywordOk, .ng = keywordOk });
        usz nextOk = instructions.len() - 1;

        expression.emitByteCode(instructions, nextOk, nextNg);
        nextNg = instructions.len() - 1;

        (*instructions).push(Instruction { .type = BEGIN, .argument = index + TokenType.KEYWORD.ordinal + 1, .ok = nextNg, .ng = nextNg });
        nextNg = instructions.len() - 1;

        for (int c = 0; c < 256; c++) {
            if (expression.isValidStartingChar((char)c)) {
                keywordJumpTable[c] = nextNg;
            }
        }
    }

    (*instructions).push(Instruction { .type = JUMPTABLE, .argument = (uptr)keywordJumpTable.ptr, .ok = nextNg, .ng = nextNg });
    nextNg = instructions.len() - 1;

    (*instructions).push(Instruction { .type = SEEK, .argument = 0, .ok = nextNg, .ng = nextNg });
    nextNg = instructions.len() - 1;

    (*instructions).push(Instruction { .type = MOVPOS, .argument = 1, .ok = nextNg, .ng = nextNg });
    usz wordOk = nextNg = instructions.len() - 1;

    for (int i = 0; i < 256; i++) {
        wordJumpTable[i] = nextNg;
    }

    // Word bytecode gen
    foreach_r (index, expression : self.wordPatterns) {
        expression.emitByteCode(instructions, wordOk, nextNg);
        nextNg = instructions.len() - 1;
        for (int c = 0; c < 256; c++) {
            if (expression.isValidStartingChar((char)c)) {
                wordJumpTable[c] = nextNg;
            }
        }
    }

    (*instructions).push(Instruction { .type = JUMPTABLE, .argument = (uptr)wordJumpTable.ptr, .ok = nextNg, .ng = nextNg });
    nextNg = instructions.len() - 1;

    (*instructions).push(Instruction { .type = MOVPOS, .argument = 0, .ok = nextNg, .ng = nextNg });
    nextNg = instructions.len() - 1;

}

fn bool Lexer.isValidStartingChar(&self, char c) @dynamic {
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

    iptr[4] registers;

    usz programCounter = 0;
    Instruction inst;
    for LOOP: (;;) {
        inst = instructions[programCounter];
        io::printf("%03d : ", programCounter);
        io::print(inst.type);
        io::printf(" \t| Position : %04d [%s] | Registers : ", ctx.position, input[ctx.position:1]);
        foreach (reg : registers) {
            io::printf("%02d, ", reg);
        }
        io::printn();
        switch (inst.type) {
            case PUSH:
                stack.push(ctx);
                programCounter = inst.ok;
            case POP:
                ctx = stack.pop()!!;
                programCounter = inst.ok;
                // Peut être qu'on s'en fout
                // TODO: Penser à ramener le buffer de tokens à son état avant le push
                //       Rajouter la longueur de la liste dans le contexte
            case DROP:
                stack.pop()!!;
                programCounter = inst.ok;
            case CLASS:
                CharClass* class = (CharClass*)inst.argument;
                if (ctx.position < inputLength && class.contains(input[ctx.position])) {
                    ctx.position++;
                    programCounter = inst.ok;
                } else {
                    programCounter = inst.ng;
                }
            case STRING:
                String s = (String)((char*)inst.argument)[:inst.optArgument];
                if (s == input[ctx.position:inst.optArgument]) {
                    ctx.position += inst.optArgument;
                    programCounter = inst.ok;
                } else {
                    io::printfn("%s != %s", s, input[ctx.position:inst.optArgument]);
                    programCounter = inst.ng;
                }
            case BEGIN:
                ctx.beginTokenIndex = ctx.position;
                ctx.tokenType = (int)inst.argument;
                programCounter = inst.ok;
            case END:
                // TODO: move token type to END instruction argument
                tokens.push(Token {
                    (TokenType)ctx.tokenType,
                    input[ctx.beginTokenIndex..ctx.position - 1],
                });
                programCounter = inst.ok;
            case DOT:
                ctx.position++;
                programCounter = inst.ok;
            case RETRY:
                // io::printfn("%d / %d", ctx.position, inputLength);
                if (ctx.position >= inputLength) {
                    break LOOP;
                }
                programCounter = 0;
            case JUMPTABLE:
                usz[] jumpTable = ((ulong*)inst.argument)[:256];
                programCounter = jumpTable[input[ctx.position]];
            case MOVPOS:
                usz registerIndex = inst.argument;
                assert (registerIndex < registers.len, "Register index out of bounds");
                registers[registerIndex] = ctx.position;
                programCounter = inst.ok;
            case SEEK:
                usz registerIndex = inst.argument;
                assert (registerIndex < registers.len, "Register index out of bounds");
                ctx.position = registers[registerIndex];
                programCounter = inst.ok;
            case GREATER:
                usz firstRegisterIndex = inst.argument;
                usz secondRegisterIndex = inst.optArgument;
                assert (firstRegisterIndex < registers.len, "Register index out of bounds");
                assert (secondRegisterIndex < registers.len, "Register index out of bounds");

                if (registers[firstRegisterIndex] > registers[secondRegisterIndex]) {
                    programCounter = inst.ok;
                } else {
                    programCounter = inst.ng;
                }

            case DELTOKEN:
                assert (tokens.len() > 0, "Cannot delete tokens from because there are none yet.");
                if (inst.argument == 0) {
                    tokens.pop()!!;
                } else {
                    tokens.remove_at(tokens.len() - 1 - inst.argument);
                }
                programCounter = inst.ok;
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
    //                 &&Plus {
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

    // g = [ \n\t]*     // Whitespace
    //   | <FOREACH>    "foreach"
    //   | <FOR>        "for"
    //   | <IDENT>      [A-Za-z_][0-9A-Za-z_]
    //   | <INVALID>    .
    CharClass* identHead = charclass::newFromString("A-Za-z_");
    CharClass* identBody = charclass::newFromString("0-9A-Za-z_");
    CharClass* numberHead = charclass::newFromString("0-9");
    CharClass* numberBody = identBody;
    CharClass* whitespace = charclass::newFromString(" \r\n\t");

    Grammar g = &&Lexer {
        .keywords = {
            &&MatchString {
                .string = "foreach"
            },
            &&MatchString {
                .string = "for"
            },
        },
        .wordPatterns = {
            &&Sequence {
                .expressions = {
                    &&Plus {
                        .expression = whitespace
                    },
                }
            },
            &&ReturnSequence {
                .tokenType = TokenType.IDENT.ordinal,
                .expressions = {
                    identHead,
                    &&Star {
                        .expression = identBody
                    },
                },
            },
            // &&ReturnSequence {
            //     .tokenType = 3,
            //     .expressions = {
            //         numberHead,
            //         &&Star {
            //             .expression = numberBody
            //         },
            //     },
            // },
            &&ReturnSequence {
                .tokenType = TokenType.INVALID.ordinal,
                .expressions = {
                    DOT
                }
            },
        }
    };

    InstructionList instructions;
    instructions.temp_init();

    g.emitByteCode(&instructions, 0, 0);

    io::printn();

    // Reverse instruction order
    // Apply the reversal to all jump addresses, including the ones in jump tables
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
        io::printf("%03d: ", index);
        io::printn(*inst);
    }

    io::printn();

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

enum TokenType {
    INVALID,
    WHITESPACE,
    PUNCTUATION,
    IDENT,
    NUMERIC,
    KEYWORD,
    FOREACH,
    FOR,
}

struct Token {
    TokenType type;
    String content;
}

def TokenList = List(<Token>);

const INPUT_FILE = "resources/min.txt";
// const INPUT_FILE = "resources/input.c3";
// const INPUT_FILE = "resources/sqlite3.c";
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

    // usz tokenBegin = 0;
    // TokenList tokens;
    // tokens.new_init();
    // LexingState state = WHITESPACE;

    // CharClass* identHead = charclass::newFromString("A-Za-z_");
    // CharClass* identBody = charclass::newFromString("0-9A-Za-z_");
    // CharClass* numberHead = charclass::newFromString("0-9");
    // CharClass* numberBody = identBody;
    //
    // foreach (usz cursor, char c : input[:inputLength]) {
    //     switch (state) {
    //         case WHITESPACE:
    //         case PUNCTUATION:
    //             if (!c.isWhitespace()) {
    //                 switch {
    //                     case identHead.contains(c):
    //                         state = IDENT;
    //                         tokenBegin = cursor;
    //                     case numberHead.contains(c):
    //                         state = NUMERIC;
    //                         tokenBegin = cursor;
    //                     default:
    //                         state = PUNCTUATION;
    //                         tokens.push(Token { state, input[cursor..cursor] });
    //                 }
    //             }
    //         case IDENT:
    //             switch {
    //                 case c.isWhitespace():
    //                     tokens.push(Token { state, input[tokenBegin..cursor - 1] });
    //                     state = WHITESPACE;
    //                 case !identBody.contains(c):
    //                     tokens.push(Token { state, input[tokenBegin..cursor - 1] });
    //                     state = PUNCTUATION;
    //                     tokens.push(Token { state, input[cursor..cursor] });
    //             }
    //         case NUMERIC:
    //             switch {
    //                 case c.isWhitespace():
    //                     tokens.push(Token { state, input[tokenBegin..cursor - 1] });
    //                     state = WHITESPACE;
    //                 case !numberBody.contains(c):
    //                     tokens.push(Token { state, input[tokenBegin..cursor - 1] });
    //                     state = PUNCTUATION;
    //                     tokens.push(Token { state, input[cursor..cursor] });
    //             }
    //     }
    // }
    //
    // NanoDuration duration = clock.mark();

    // foreach (token : tokens) {
    //     io::printn(token);
    // }
    // io::printfn("Input size : %d Bytes", inputLength);
    // io::printfn("Lexing finished in %s", duration);
    //
    // io::printfn("Recognized %d tokens", tokens.len());
    // io::printfn("Speed : %.02f Million tokens per second", tokens.len()/duration.to_sec()/1_000_000);


    TokenList vmTokens = testVm(input, inputLength);

    foreach (token : vmTokens) {
        io::printn(token);
    }
}

// Old Ideas
// Parsing lexer grammar file

// enum States {
//     OUTER,     // Expect Keyword, then { --> BLOCK(Keyword) OR EOF --> DONE
//     BLOCK,     // Expect Statement --> STATEMENT OR } --> OUTER
//     STATEMENT, // Depends on the keyword --> BLOCK
//     DONE,      // Good Job
// }

// Line trimming (might be useful for frontend of the parser generator

// while (try line = io::treadline(stream)) {
//     String trimmedLine = line.trim();
//     io::printfn("%03d | %s", trimmedLine.len, trimmedLine);
// }
