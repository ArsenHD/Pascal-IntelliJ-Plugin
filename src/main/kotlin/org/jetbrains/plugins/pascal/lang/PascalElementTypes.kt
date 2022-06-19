package org.jetbrains.plugins.pascal.lang

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.annotations.NonNls


class PascalElementType(@NonNls debugName: String) : IElementType(debugName, PascalLanguage) {
    companion object {
        val PASCAL_FILE = IFileElementType(PascalLanguage)
        val PASCAL_STUB_FILE = PascalStubFileElementType()

        val PROGRAM_HEADER = PascalElementType("PROGRAM_HEADER")

        val USES_CLAUSE = PascalElementType("USES_CLAUSE")
        val USES_CLAUSE_ENTRY = PascalElementType("USES_CLAUSE_ENTRY")

        val BLOCK = PascalElementType("BLOCK")

        val BLOCK_DECLARATION_PART = PascalElementType("BLOCK_DECLARATION_PART")

        val CONSTANT_DECLARATIONS = PascalElementType("CONSTANT_DECLARATIONS")
        val VARIABLE_DECLARATIONS = PascalElementType("VARIABLE_DECLARATIONS")

        val CONSTANT_DECLARATION = PascalElementType("CONSTANT_DECLARATION")
        val TYPED_CONSTANT_DECLARATION = PascalElementType("TYPED_CONSTANT_DECLARATION")

        val VARIABLE_DECLARATION = PascalElementType("VARIABLE_DECLARATION")

        val FUNCTION_DECLARATION = PascalElementType("FUNCTION_DECLARATION")
        val PROCEDURE_DECLARATION = PascalElementType("PROCEDURE_DECLARATION")

        val PARAMETER_LIST = PascalElementType("PARAMETER_LIST")
        val PARAMETER = PascalElementType("PARAMETER")

        val BLOCK_STATEMENT_PART = PascalElementType("BLOCK_STATEMENT_PART")

        // statements

        // simple statements
        val ASSIGNMENT = PascalElementType("ASSIGNMENT")

        // structured statements
        val COMPOUND_STATEMENT = PascalElementType("COMPOUND_STATEMENT")

        val IF = PascalElementType("IF")
        val CONDITION = PascalElementType("CONDITION")
        val THEN = PascalElementType("THEN")
        val ELSE = PascalElementType("ELSE")

        val FOR_LOOP = PascalElementType("FOR_LOOP")
        val CONTROL_VARIABLE = PascalElementType("CONTROL_VARIABLE")
        val LOOP_RANGE = PascalElementType("LOOP_RANGE")
        val LOOP_BODY = PascalElementType("LOOP_BODY")

        val WHILE_LOOP = PascalElementType("WHILE_LOOP")

        val REPEAT_UNTIL = PascalElementType("REPEAT_UNTIL")

        // expressions

        val OPERATION_REFERENCE = PascalElementType("OPERATION_REFERENCE")

        val BINARY_EXPRESSION = PascalElementType("BINARY_EXPRESSION")
        val PREFIX_EXPRESSION = PascalElementType("PREFIX_EXPRESSION")
        val PARENTHESIZED = PascalElementType("PARENTHESIZED")

        val VARIABLE_REFERENCE = PascalElementType("VARIABLE_REFERENCE")
        val CALL_EXPRESSION = PascalElementType("CALL_EXPRESSION")
        val TRUE_CONSTANT = PascalElementType("TRUE_CONSTANT")
        val FALSE_CONSTANT = PascalElementType("FALSE_CONSTANT")
        val UNSIGNED_INT_NUMBER = PascalElementType("UNSIGNED_INT_NUMBER")
        val UNSIGNED_FP_NUMBER = PascalElementType("UNSIGNED_FP_NUMBER")
        val CHARACTER_STRING = PascalElementType("CHARACTER_STRING")
        val NIL_REFERENCE = PascalElementType("NIL_REFERENCE")

        val ARGUMENT_LIST = PascalElementType("ARGUMENT_LIST")

        // types
        val TYPE_REFERENCE = PascalElementType("TYPE_REFERENCE")
        val SUBRANGE_TYPE = PascalElementType("SUBRANGE_TYPE")

        val ARRAY_DIMENSIONS = PascalElementType("ARRAY_DIMENSIONS")

        val ARRAY_CONTENT_TYPE = PascalElementType("ARRAY_CONTENT_TYPE")

        val GARBAGE_AT_THE_END_OF_FILE = PascalElementType("GARBAGE_AT_THE_END_OF_FILE")

		val EXPRESSIONS = TokenSet.create(
            BINARY_EXPRESSION,
            PREFIX_EXPRESSION,
            VARIABLE_REFERENCE, CALL_EXPRESSION,
            TRUE_CONSTANT, FALSE_CONSTANT,
            UNSIGNED_INT_NUMBER, UNSIGNED_FP_NUMBER,
            CHARACTER_STRING, NIL_REFERENCE
        )


		val STATEMENTS = TokenSet.create(
            ASSIGNMENT, CALL_EXPRESSION, COMPOUND_STATEMENT, IF, FOR_LOOP, WHILE_LOOP, REPEAT_UNTIL
        )

        fun createElement(node: ASTNode): PsiElement {
            return when (node.elementType) {
                PROGRAM_HEADER -> PascalPsiElement(node)
                USES_CLAUSE -> PascalPsiElement(node)
                USES_CLAUSE_ENTRY -> PascalPsiElement(node)
                BLOCK -> PascalBlock(node)
                BLOCK_DECLARATION_PART -> PascalBlockDeclarationPart(node)
                CONSTANT_DECLARATIONS -> PascalPsiElement(node)
                VARIABLE_DECLARATIONS -> PascalVariableDeclarationPart(node)
                CONSTANT_DECLARATION -> PascalPsiElement(node)
                TYPED_CONSTANT_DECLARATION -> PascalPsiElement(node)
                VARIABLE_DECLARATION -> PascalVariableDeclaration(node)
                FUNCTION_DECLARATION -> PascalFunction(node)
                PROCEDURE_DECLARATION -> PascalProcedure(node)
                PARAMETER_LIST -> PascalParameterList(node)
                PARAMETER -> PascalParameter(node)
                BLOCK_STATEMENT_PART -> PascalBlockStatementPart(node)
                ASSIGNMENT -> PascalPsiElement(node)
                COMPOUND_STATEMENT -> PascalPsiElement(node)
                IF -> PascalPsiElement(node)
                CONDITION -> PascalPsiElement(node)
                THEN -> PascalPsiElement(node)
                ELSE -> PascalPsiElement(node)
                FOR_LOOP -> PascalForLoop(node)
                CONTROL_VARIABLE -> PascalForLoopControlVariable(node)
                LOOP_RANGE -> PascalPsiElement(node)
                LOOP_BODY -> PascalPsiElement(node)
                WHILE_LOOP -> PascalPsiElement(node)
                REPEAT_UNTIL -> PascalPsiElement(node)
                OPERATION_REFERENCE -> PascalPsiElement(node)
                BINARY_EXPRESSION -> PascalBinaryExpression(node)
                PREFIX_EXPRESSION -> PascalPrefixExpression(node)
                PARENTHESIZED -> PascalPsiElement(node)
                VARIABLE_REFERENCE -> PascalVariableAccess(node)
                CALL_EXPRESSION -> PascalCallExpression(node)
                TRUE_CONSTANT -> PascalTrueConstant(node)
                FALSE_CONSTANT -> PascalFalseConstant(node)
                UNSIGNED_INT_NUMBER -> PascalUnsignedIntNumber(node)
                UNSIGNED_FP_NUMBER -> PascalUnsignedFloatNumber(node)
                CHARACTER_STRING -> PascalCharacterString(node)
                NIL_REFERENCE -> PascalNilReference(node)
                ARGUMENT_LIST -> PascalArgumentList(node)
                TYPE_REFERENCE -> PascalTypeRef(node)
                SUBRANGE_TYPE -> PascalSubrangeType(node)
                ARRAY_DIMENSIONS -> PascalArrayDimensions(node)
                ARRAY_CONTENT_TYPE -> PascalPsiElement(node)
                GARBAGE_AT_THE_END_OF_FILE -> PascalPsiElement(node)
                else -> throw IllegalArgumentException("Unknown elementType: " + node.elementType)
            }
        }

    }

}
