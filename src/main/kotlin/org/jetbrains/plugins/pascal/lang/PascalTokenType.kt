package org.jetbrains.plugins.pascal.lang

import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.annotations.NonNls

class PascalTokenType(@NonNls debugName: String) : IElementType(debugName, PascalLanguage) {
    override fun toString(): String {
        return "PascalTokenType." + super.toString()
    }

    companion object {
        val PROGRAM = PascalTokenType("PROGRAM")

        val ID = PascalTokenType("ID")

        val USES = PascalTokenType("USES")

        val CONST = PascalTokenType("CONST")

        val FUNCTION = PascalTokenType("FUNCTION")
        val PROCEDURE = PascalTokenType("PROCEDURE")

        val VAR = PascalTokenType("VAR")

        val DO = PascalTokenType("DO")

        val OF = PascalTokenType("OF")

        // types
        val ARRAY = PascalTokenType("ARRAY")
        val STRING = PascalTokenType("STRING")
        val UNSIGNED_INT = PascalTokenType("UNSIGNED_INT")
        val UNSIGNED_FP_NUMBER = PascalTokenType("UNSIGNED_FP_NUMBER")

        // control flow
        val IF = PascalTokenType("IF")
        val THEN = PascalTokenType("THEN")
        val ELSE = PascalTokenType("ELSE")
        val FOR = PascalTokenType("FOR")
        val TO = PascalTokenType("TO")
        val DOWNTO = PascalTokenType("DOWNTO")
        val UNTIL = PascalTokenType("UNTIL")
        val WHILE = PascalTokenType("WHILE")
        val REPEAT = PascalTokenType("REPEAT")

        // scope
        val BEGIN = PascalTokenType("BEGIN")
        val END = PascalTokenType("END")

        // comparisons
        val LT = PascalTokenType("LT")
        val LEQ = PascalTokenType("LEQ")
        val GT = PascalTokenType("GT")
        val GEQ = PascalTokenType("GEQ")
        val EQ = PascalTokenType("EQ")
        val NEQ = PascalTokenType("NEQ")

        // arithmetic operators
        val PLUS = PascalTokenType("PLUS")
        val MINUS = PascalTokenType("MINUS")
        val MULT = PascalTokenType("MULT")
        val DIVIDE = PascalTokenType("DIVIDE")
        val DIV = PascalTokenType("DIV")
        val MOD = PascalTokenType("MOD")

        // logical operators
        val NOT = PascalTokenType("NOT")
        val AND = PascalTokenType("AND")
        val OR = PascalTokenType("OR")
        val XOR = PascalTokenType("XOR")

        // bitwise shifts
        val SHL = PascalTokenType("SHL")
        val SHR = PascalTokenType("SHR")

        val IN = PascalTokenType("IN")

        val TRUE = PascalTokenType("TRUE")
        val FALSE = PascalTokenType("FALSE")

        val NIL = PascalTokenType("NIL")

        val ONE_LINE_COMMENT = PascalTokenType("ONE_LINE_COMMENT")
        val PARENTHESES_COMMENT = PascalTokenType("PARENTHESES_COMMENT")
        val BRACES_COMMENT = PascalTokenType("BRACES_COMMENT")

        val NEW_LINE = PascalTokenType("NEW_LINE")

        val COLON = PascalTokenType("COLON")
        val SEMICOLON = PascalTokenType("SEMICOLON")
        val DOT = PascalTokenType("DOT")
        val DOUBLE_DOT = PascalTokenType("DOUBLE_DOT")
        val LBRACKET = PascalTokenType("LBRACKET")
        val RBRACKET = PascalTokenType("RBRACKET")
        val LPAREN = PascalTokenType("LPAREN")
        val RPAREN = PascalTokenType("RPAREN")
        val LBRACE = PascalTokenType("LBRACE")
        val RBRACE = PascalTokenType("RBRACE")
        val COMMA = PascalTokenType("COMMA")
        val ASSIGN = PascalTokenType("ASSIGN")

        val COMMENTS = TokenSet.create(ONE_LINE_COMMENT, PARENTHESES_COMMENT, BRACES_COMMENT)
        val STRINGS = TokenSet.create(STRING)
        val IDENTS = TokenSet.create(ID)
        val OPERATIONS = TokenSet.create(LT, LEQ, GT, GEQ, EQ, NEQ, DIV, MOD, NOT, AND, OR, XOR, SHL, SHR)
        val KEYWORDS = TokenSet.create(
            PROGRAM, USES, FUNCTION, PROCEDURE, VAR, ARRAY, STRING,
            IF, THEN, ELSE, FOR, TO, DOWNTO, UNTIL, WHILE, REPEAT,
            BEGIN, END, EQ, DIV, MOD, NOT, AND, OR, XOR, SHL, SHR,
            FALSE, TRUE, NIL
        )

        val STATEMENT_FIRST = TokenSet.create(
            IF, FOR, WHILE, REPEAT, BEGIN
        )

        val EXPRESSION_FIRST = TokenSet.create(
            PLUS, MINUS, LPAREN, TRUE, FALSE, NIL,
            UNSIGNED_INT, UNSIGNED_FP_NUMBER, STRING, ID, NOT
        )
    }
}
