module parsing::charclass;

import std::io;

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

fn void CharClass.emitByteCode(&self, InstructionList* instructions, usz ok, usz ng) @dynamic {
    instructions.push(Instruction {
        .type = CLASS,
        .argument = (uptr)self,
        .ok = ok,
        .ng = ng,
    });
}

fn void testCharClass() @test {
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

fn void testCharClassByteCode() @test {
    InstructionList instructions;
    instructions.new_init();
    Grammar class = newFromString("a-z");
    class.emitByteCode(&instructions, 0, 0);
    assert(instructions.len() == 1);
    assert(instructions[0].equals(Instruction { .type = CLASS, .argument = (uptr)class.ptr, .ok = 0, .ng = 0 }));
    assert(((CharClass*)instructions[0].argument).contains('c'));
    assert(!((CharClass*)instructions[0].argument).contains('A'));
}
