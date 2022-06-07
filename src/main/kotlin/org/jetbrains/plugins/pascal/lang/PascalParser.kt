package org.jetbrains.plugins.pascal.lang

import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiBuilder
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.PROGRAM
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.ID
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.USES
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.CONST
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.FUNCTION
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.PROCEDURE
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.VAR
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DO
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.OF
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.ARRAY
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.STRING
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.UNSIGNED_INT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.IF
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.THEN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.ELSE
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.FOR
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.TO
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DOWNTO
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.UNTIL
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.WHILE
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.REPEAT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.BEGIN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.END
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.LT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.LEQ
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.GT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.GEQ
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.EQ
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.NEQ
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.PLUS
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.MINUS
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.MULT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DIVIDE
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DIV
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.MOD
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.NOT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.AND
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.OR
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.XOR
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.SHL
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.SHR
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.IN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.COLON
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.SEMICOLON
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DOT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.DOUBLE_DOT
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.LBRACKET
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.RBRACKET
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.LPAREN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.RPAREN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.COMMA
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.ASSIGN
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.COMMENTS
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.STATEMENT_FIRST
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.EXPRESSION_FIRST
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.FALSE
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.UNSIGNED_FP_NUMBER
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.NIL
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.STRINGS
import org.jetbrains.plugins.pascal.lang.PascalTokenType.Companion.TRUE

class PascalParser : PsiParser {
    override fun parse(root: IElementType, builder: PsiBuilder): ASTNode {
        InnerParser(builder).parseFile()
        return builder.treeBuilt
    }

    class InnerParser(private val builder: PsiBuilder) {

        fun parseFile() {
            val mark = builder.mark()

            parseProgramHeader()

            if (at(USES)) {
                parseUsesClause()
            }

            parseBlock()

            expectAdvance(DOT, "'.'")

            if (currentToken != null) {
                parseGarbage()
            }

            mark.done(PascalElementType.PASCAL_FILE)
        }

        private fun parseGarbage() {
            val mark = builder.mark()
            builder.error("Expected program body")

            while (currentToken != null) {
                when(currentToken) {
                    FUNCTION -> parseFunctionDeclaration()
                    PROCEDURE -> parseProcedureDeclaration()
                    BEGIN -> parseBlock()
                    VAR -> parseVariableDeclarations()
                    CONST -> parseConstDeclarations()
                    else -> advance()
                }
            }
            mark.done(PascalElementType.GARBAGE_AT_THE_END_OF_FILE)
        }

        private fun parseProgramHeader() {
            val mark = builder.mark()

            if (expectAdvance(PROGRAM, "'program'")) {
                expectAdvance(ID, "program header")
                expectAdvance(SEMICOLON, "';'")
            }

            mark.done(PascalElementType.PROGRAM_HEADER)
        }

        private fun parseUsesClause() {
            val mark = builder.mark()

            assertAdvance(USES)
            while (at(ID)) {
                val entryMark = builder.mark()
                assertAdvance(ID)
                if (at(IN)) {
                    assertAdvance(IN)
                    expectAdvance(STRING, "unit filename")
                }
                entryMark.done(PascalElementType.USES_CLAUSE_ENTRY)
                if (at(SEMICOLON)) {
                    break
                }
                expectAdvance(COMMA, "comma")
            }
            expectAdvance(SEMICOLON, "';'")

            mark.done(PascalElementType.USES_CLAUSE)
        }

        private fun parseBlock() {
            val mark = builder.mark()
            parseBlockDeclarationPart()
            parseBlockStatementPart()
            mark.done(PascalElementType.BLOCK)
        }

        private fun parseBlockDeclarationPart() {
            val mark = builder.mark()
            while (true) {
                when (currentToken) {
                    CONST -> parseConstDeclarations()
                    VAR -> parseVariableDeclarations()
                    FUNCTION -> parseFunctionDeclaration()
                    PROCEDURE -> parseProcedureDeclaration()
                    else -> break
                }
            }
            mark.done(PascalElementType.BLOCK_DECLARATION_PART)
        }

        private fun parseConstDeclarations() {
            val mark = builder.mark()

            assertAdvance(CONST)

            if (!at(ID)) {
                errorAdvance("constant name")
            }

            while (at(ID)) {
                val constMark = builder.mark()
                assertAdvance(ID)
                var constType = PascalElementType.CONSTANT_DECLARATION
                if (at(COLON)) {
                    constType = PascalElementType.TYPED_CONSTANT_DECLARATION
                    assertAdvance(COLON)
                    parseType()
                }
                if (expectAdvance(EQ, "'='")) {
                    parseExpression()
                }
                constMark.done(constType)
                expectAdvance(SEMICOLON, "';'")
            }

            mark.done(PascalElementType.CONSTANT_DECLARATIONS)
        }

        private fun parseVariableDeclarations() {
            val mark = builder.mark()

            assertAdvance(VAR)

            if (!at(ID)) {
                errorAdvance("variable name")
            }

            while (at(ID)) {
                val variableMark = builder.mark()
                assertAdvance(ID)
                if (expectAdvance(COLON, "variable type")) {
                    parseType()
                }
                if (at(EQ)) {
                    assertAdvance(EQ)
                    parseExpression()
                }
                variableMark.done(PascalElementType.VARIABLE_DECLARATION)
                expectAdvance(SEMICOLON, ";")
            }

            mark.done(PascalElementType.VARIABLE_DECLARATIONS)
        }

        private fun parseFunctionDeclaration() {
            val mark = builder.mark()

            assertAdvance(FUNCTION)

            expectAdvance(ID, "function name")
            parseParameterList()

            expectAdvance(COLON, "':'")
            parseType()

            parseBlock()

            expectAdvance(SEMICOLON, "';'")

            mark.done(PascalElementType.FUNCTION_DECLARATION)
        }

        private fun parseProcedureDeclaration() {
            val mark = builder.mark()

            assertAdvance(PROCEDURE)

            expectAdvance(ID, "procedure name")
            parseParameterList()

            parseBlock()

            expectAdvance(SEMICOLON, "';'")

            mark.done(PascalElementType.PROCEDURE_DECLARATION)
        }

        private fun parseParameterList() {
            val mark = builder.mark()

            if (expectAdvance(LPAREN, "parameters declaration")) {
                if (!at(RPAREN)) {
                    while (true) {
                        while (at(COMMA)) {
                            errorAdvance("parameter declaration")
                        }
                        parseParameter()
                        if (at(RPAREN)) {
                            break
                        }
                        if (!expectAdvance(COMMA, "comma")) {
                            break
                        }
                    }
                }
                expectAdvance(RPAREN, "')'")
            }

            mark.done(PascalElementType.PARAMETER_LIST)
        }

        private fun parseParameter() {
            val mark = builder.mark()

            if (expectAdvance(ID, "parameter name")) {
                expectAdvance(COLON, "parameter type")
                parseType()
            }

            mark.done(PascalElementType.PARAMETER)
        }

        private fun parseBlockStatementPart() {
            val mark = builder.mark()
            parseCompoundStatement()
            mark.done(PascalElementType.BLOCK_STATEMENT_PART)
        }

        private fun parseExpression() {
            if (currentToken !in EXPRESSION_FIRST) {
                builder.error("Expecting an expression")
                return
            }
            parseBinaryExpression()
        }

        private fun parseBinaryExpression() {
            val latestMark = parseSimpleExpression()

            if (currentToken in Precedence.FOURTH) {
                val operator = builder.mark()
                advance()
                operator.done(PascalElementType.OPERATION_REFERENCE)

                val binaryExpression = latestMark.precede()
                parseSimpleExpression()
                binaryExpression.done(PascalElementType.BINARY_EXPRESSION)
            }
        }

        private fun parseSimpleExpression(): PsiBuilder.Marker {
            val mark = builder.mark()
            var latestMark = if (at(PLUS) || at(MINUS)) {
                val operator = builder.mark()
                advance()
                operator.done(PascalElementType.OPERATION_REFERENCE)

                parseTerm()
                mark.done(PascalElementType.PREFIX_EXPRESSION)
                mark
            } else {
                mark.drop()
                parseTerm()
            }

            while (currentToken in Precedence.THIRD) {
                val operator = builder.mark()
                advance()
                operator.done(PascalElementType.OPERATION_REFERENCE)

                val binaryExpression = latestMark.precede()
                parseTerm()
                binaryExpression.done(PascalElementType.BINARY_EXPRESSION)
                latestMark = binaryExpression
            }

            return latestMark
        }

        private fun parseTerm(): PsiBuilder.Marker {
            var latestMark = parseFactor()

            while (currentToken in Precedence.SECOND) {
                val operator = builder.mark()
                advance()
                operator.done(PascalElementType.OPERATION_REFERENCE)

                val binaryExpression = latestMark.precede()
                parseFactor()
                binaryExpression.done(PascalElementType.BINARY_EXPRESSION)
                latestMark = binaryExpression
            }

            return latestMark
        }

        private fun parseFactor(): PsiBuilder.Marker {
            var mark = builder.mark()

            when (currentToken) {
                LPAREN -> {
                    assertAdvance(LPAREN)
                    parseExpression()
                    expectAdvance(RPAREN, "')'")
                    mark.done(PascalElementType.PARENTHESIZED)
                }
                ID -> {
                    val isCall = tryParseCallExpression(mark)
                    if (!isCall) {
                        // existing mark has been rolled back to, so we create it again
                        mark = builder.mark()
                        assertAdvance(ID)
                        mark.done(PascalElementType.VARIABLE_REFERENCE)
                    }
                }
                UNSIGNED_INT, UNSIGNED_FP_NUMBER, STRING -> {
                    mark.drop()
                    mark = parseUnsignedConstant()
                }
                FALSE -> {
                    assertAdvance(FALSE)
                    mark.done(PascalElementType.FALSE_CONSTANT)
                }
                TRUE -> {
                    assertAdvance(TRUE)
                    mark.done(PascalElementType.TRUE_CONSTANT)
                }
                NIL -> {
                    assertAdvance(NIL)
                    mark.done(PascalElementType.NIL_REFERENCE)
                }
                NOT -> {
                    val operator = builder.mark()
                    assertAdvance(NOT)
                    operator.done(PascalElementType.OPERATION_REFERENCE)

                    parseFactor()
                    mark.done(PascalElementType.PREFIX_EXPRESSION)
                }
                else -> {
                    advance()
                    mark.error("Expression expected")
                }
            }

            return mark
        }

        private fun parseUnsignedConstant(): PsiBuilder.Marker {
            val mark = builder.mark()

            when (currentToken) {
                UNSIGNED_INT -> {
                    advance()
                    mark.done(PascalElementType.UNSIGNED_INT_NUMBER)
                } UNSIGNED_FP_NUMBER -> {
                    advance()
                    mark.done(PascalElementType.UNSIGNED_FP_NUMBER)
                }
                STRING -> {
                    advance()
                    mark.done(PascalElementType.CHARACTER_STRING)
                }
                else -> mark.error("Unsigned constant expected")
            }

            return mark
        }

        private fun parseType() {
            val mark = builder.mark()
            if (at(ARRAY)) {
                assertAdvance(ARRAY)

                if (at(LBRACKET)) {
                    val dimensionsMark = builder.mark()

                    assertAdvance(LBRACKET)

                    if (at(RBRACKET)) {
                        assertAdvance(RBRACKET)
                        dimensionsMark.error("Array types with zero dimensions are not allowed")
                    } else {
                        while (true) {
                            while (at(COMMA)) {
                                errorAdvance("indices range")
                            }
                            parseSubrangeType()
                            if (at(RBRACKET)) {
                                break
                            }
                            if (!expectAdvance(COMMA, "comma")) {
                                break
                            }
                        }
                        expectAdvance(RBRACKET, "']'")
                    }

                    dimensionsMark.done(PascalElementType.ARRAY_DIMENSIONS)
                }

                val markContentType = builder.mark()
                expectAdvance(OF, "'of'")
                parseType() // content type
                markContentType.done(PascalElementType.ARRAY_CONTENT_TYPE)

                mark.done(PascalElementType.TYPE_REFERENCE)
            } else {
                expectAdvance(ID, "type identifier")
                mark.done(PascalElementType.TYPE_REFERENCE)
            }
        }

        private fun parseSubrangeType() {
            val mark = builder.mark()

            expectAdvance(UNSIGNED_INT, "left range bound")
            expectAdvance(DOUBLE_DOT, "'..'")
            expectAdvance(UNSIGNED_INT, "right range bound")

            mark.done(PascalElementType.SUBRANGE_TYPE)
        }

        private fun parseCompoundStatement() {
            val mark = builder.mark()

            if (expectAdvance(BEGIN, "'begin'")) {
                while (!eof() && !at(END)) {
                    val statementParsed = tryParseStatement()
                    if (!statementParsed) {
                        errorAdvance("statement")
                        continue
                    }
                    if (lookAhead(1) == END && at(SEMICOLON)) {
                        assertAdvance(SEMICOLON)
                    }
                    if (at(END)) {
                        break
                    }
                    expectAdvance(SEMICOLON, "';'")
                }
                expectAdvance(END, "'end'")
            }

            mark.done(PascalElementType.COMPOUND_STATEMENT)
        }

        private fun tryParseStatement(): Boolean {
            if (tryParseAssignment()) return true
            if (tryParseCallExpression()) return true

            if (currentToken in STATEMENT_FIRST) {
                when (currentToken) {
                    IF -> parseIfStatement()
                    FOR -> parseForLoop()
                    WHILE -> parseWhileLoop()
                    REPEAT -> parseRepeatUntil()
                    BEGIN -> parseCompoundStatement()
                }
                return true
            }
            return false
        }

        private fun tryParseAssignment(): Boolean {
            val mark = builder.mark()
            if (at(ID)) {
                val variableReferenceMark = builder.mark()
                assertAdvance(ID)
                variableReferenceMark.done(PascalElementType.VARIABLE_REFERENCE)

                if (at(ASSIGN)) {
                    assertAdvance(ASSIGN)
                    parseExpression()
                    mark.done(PascalElementType.ASSIGNMENT)
                    return true
                }
            }
            mark.rollbackTo()
            return false
        }

        private fun tryParseCallExpression(mark: PsiBuilder.Marker = builder.mark()): Boolean {
            if (at(ID)) {
                assertAdvance(ID)
                if (at(LPAREN)) {
                    parseArgumentsList()
                    mark.done(PascalElementType.CALL_EXPRESSION)
                    return true
                }
            }
            mark.rollbackTo()
            return false
        }

        private fun parseArgumentsList() {
            val mark = builder.mark()

            if (expectAdvance(LPAREN, "argument list")) {
                if (!at(RPAREN)) {
                    while (true) {
                        while (at(COMMA)) {
                            errorAdvance("argument")
                        }
                        parseArgument()
                        if (at(RPAREN)) {
                            break
                        }
                        if (!expectAdvance(COMMA, "comma")) {
                            break
                        }
                    }
                }
                expectAdvance(RPAREN, "')'")
            }

            mark.done(PascalElementType.ARGUMENT_LIST)
        }

        private fun parseArgument() {
            parseExpression()
        }

        private fun parseIfStatement() {
            val mark = builder.mark()
            if (expectAdvance(IF, "'if'")) {
                val conditionMark = builder.mark()
                parseExpression() // condition
                conditionMark.done(PascalElementType.CONDITION)

                val thenMark = builder.mark()
                expectAdvance(THEN, "'then'")
                tryParseStatement()
                thenMark.done(PascalElementType.THEN)

                if (at(ELSE)) {
                    val elseMark = builder.mark()
                    assertAdvance(ELSE)
                    tryParseStatement()
                    elseMark.done(PascalElementType.ELSE)
                }
            }
            mark.done(PascalElementType.IF)
        }

        private fun parseForLoop() {
            val mark = builder.mark()

            if (expectAdvance(FOR, "'for'")) {
                val controlVariableMark = builder.mark()
                expectAdvance(ID, "control variable name")
                controlVariableMark.done(PascalElementType.CONTROL_VARIABLE)

                expectAdvance(ASSIGN, "':='")

                val loopRangeMark = builder.mark()
                parseExpression() // initial value
                if (at(TO) || at(DOWNTO)) {
                    advance()
                }
                parseExpression() // final value
                loopRangeMark.done(PascalElementType.LOOP_RANGE)

                val loopBodyMark = builder.mark()
                expectAdvance(DO, "'do'")
                tryParseStatement()
                loopBodyMark.done(PascalElementType.LOOP_BODY)
            }

            mark.done(PascalElementType.FOR_LOOP)
        }

        private fun parseWhileLoop() {
            val mark = builder.mark()

            if (expectAdvance(WHILE, "'while'")) {
                val conditionMark = builder.mark()
                parseExpression() // condition
                conditionMark.done(PascalElementType.CONDITION)

                val loopBodyMark = builder.mark()
                expectAdvance(DO, "'do'")
                tryParseStatement()
                loopBodyMark.done(PascalElementType.LOOP_BODY)
            }

            mark.done(PascalElementType.WHILE_LOOP)
        }

        private fun parseRepeatUntil() {
            val mark = builder.mark()

            if (expectAdvance(REPEAT, "'repeat'")) {
                val loopBodyMark = builder.mark()
                if (!at(UNTIL)) {
                    while (true) {
                        val statementParsed = tryParseStatement()
                        if (!statementParsed) {
                            errorAdvance("statement")
                        }
                        if (lookAhead(1) == UNTIL && at(SEMICOLON)) {
                            assertAdvance(SEMICOLON)
                        }
                        if (at(UNTIL)) {
                            break
                        }
                        expectAdvance(SEMICOLON, "';'")
                    }
                }
                loopBodyMark.done(PascalElementType.LOOP_BODY)

                val conditionMark = builder.mark()
                expectAdvance(UNTIL, "'until'")
                parseExpression() // condition
                conditionMark.done(PascalElementType.CONDITION)
            }

            mark.done(PascalElementType.REPEAT_UNTIL)
        }

        private val currentToken: IElementType?
            get() = builder.tokenType

        private fun at(tokenType: IElementType): Boolean {
            return currentToken == tokenType
        }

        private fun lookAhead(steps: Int): IElementType? {
            return builder.lookAhead(steps)
        }

        private fun expectAdvance(expectedToken: PascalTokenType, expectedName: String): Boolean {
            if (currentToken == expectedToken) {
                advance()
                return true
            }
            builder.error("Expected $expectedName")
            return false
        }

        private fun eof(): Boolean {
            return builder.eof()
        }

        private fun assertAdvance(tokenType: IElementType) {
            assert(currentToken == tokenType)
            advance()
        }

        private fun advance(): IElementType? {
            val token = currentToken
            builder.advanceLexer()
            while (currentToken == TokenType.BAD_CHARACTER) {
                val badMark = builder.mark()
                builder.advanceLexer()
                badMark.error("Unexpected character")
            }
            return token
        }

        private fun errorAdvance(expectedName: String) {
            val mark = builder.mark()
            advance()
            mark.error("Expected $expectedName")
        }

        private enum class Precedence(val tokens: TokenSet) {
            FIRST(NOT),
            SECOND(MULT, DIVIDE, DIV, MOD, AND, SHL, SHR),
            THIRD(PLUS, MINUS, OR, XOR),
            FOURTH(LT, LEQ, GT, GEQ, EQ, NEQ, IN);

            constructor(vararg tokens: PascalTokenType) : this(TokenSet.create(*tokens))

            operator fun contains(token: IElementType?): Boolean {
                return token in tokens
            }
        }
    }
}

class PascalParserDefinition : ParserDefinition {
    override fun createLexer(project: Project?): Lexer {
        return PascalLexer()
    }

    override fun createParser(project: Project?): PsiParser {
        return PascalParser()
    }

    override fun getFileNodeType(): IFileElementType {
        return PascalElementType.PASCAL_FILE
    }

    override fun getCommentTokens(): TokenSet {
        return COMMENTS
    }

    override fun getStringLiteralElements(): TokenSet {
        return STRINGS
    }

    override fun createElement(node: ASTNode): PsiElement {
        return PascalElementType.createElement(node)
    }

    override fun createFile(viewProvider: FileViewProvider): PsiFile {
        return PascalFile(viewProvider)
    }
}
