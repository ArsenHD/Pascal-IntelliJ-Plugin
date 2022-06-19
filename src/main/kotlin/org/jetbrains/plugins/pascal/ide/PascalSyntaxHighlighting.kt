package org.jetbrains.plugins.pascal.ide

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighter
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory
import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.pascal.lang.PascalLexer
import org.jetbrains.plugins.pascal.lang.PascalTokenType

enum class PascalTextAttributeKeys(humanName: String, fallback: TextAttributesKey) {
    ONE_LINE_COMMENT("One-line comment", DefaultLanguageHighlighterColors.DOC_COMMENT),
    PARENTHESES_COMMENT("Parentheses comment", DefaultLanguageHighlighterColors.DOC_COMMENT),
    BRACES_COMMENT("Braces comment", DefaultLanguageHighlighterColors.DOC_COMMENT),
    STRING("String//String text", DefaultLanguageHighlighterColors.STRING),
    INT("Number//Integer", DefaultLanguageHighlighterColors.NUMBER),
    FP_NUMBER("Number//Floating point", DefaultLanguageHighlighterColors.NUMBER),

    KEYWORD("Keyword", DefaultLanguageHighlighterColors.KEYWORD),

    DOT("Braces and Operators//Dot", DefaultLanguageHighlighterColors.DOT),
    COMMA("Braces and Operators//Comma", DefaultLanguageHighlighterColors.COMMA),
    SEMICOLON("Braces and Operators//Semicolon", DefaultLanguageHighlighterColors.SEMICOLON),

    PARENTHESES("Braces and Operators//Parentheses", DefaultLanguageHighlighterColors.PARENTHESES),
    BRACKETS("Braces and Operators//Brackets", DefaultLanguageHighlighterColors.BRACKETS),
    BRACES("Braces and Operators//Braces", DefaultLanguageHighlighterColors.BRACES),

    COLON("Braces and Operators//Colon", DefaultLanguageHighlighterColors.DOT),
    EQ("Braces and Operators//Equal Sign", DefaultLanguageHighlighterColors.DOT),
    ASSIGN("Braces and Operators//Assign", DefaultLanguageHighlighterColors.DOT),

    USER_TYPE("Types//User Type", DefaultLanguageHighlighterColors.CONSTANT);

    val key = TextAttributesKey.createTextAttributesKey("Pascal.$name", fallback)
    val descriptor = AttributesDescriptor(humanName, key)
}

class PascalSyntaxHighlighter : SyntaxHighlighterBase() {
    companion object {
        val keys1 = HashMap<IElementType, TextAttributesKey>()

        init
        {
            fillMap(keys1, PascalTokenType.KEYWORDS, PascalTextAttributeKeys.KEYWORD.key)

            keys1[PascalTokenType.UNSIGNED_INT] = PascalTextAttributeKeys.INT.key
            keys1[PascalTokenType.UNSIGNED_FP_NUMBER] = PascalTextAttributeKeys.FP_NUMBER.key
            keys1[PascalTokenType.STRING] = PascalTextAttributeKeys.STRING.key

            keys1[PascalTokenType.ONE_LINE_COMMENT] = PascalTextAttributeKeys.ONE_LINE_COMMENT.key
            keys1[PascalTokenType.PARENTHESES_COMMENT] = PascalTextAttributeKeys.PARENTHESES_COMMENT.key
            keys1[PascalTokenType.BRACES_COMMENT] = PascalTextAttributeKeys.BRACES_COMMENT.key

            keys1[PascalTokenType.LPAREN] = PascalTextAttributeKeys.PARENTHESES.key
            keys1[PascalTokenType.RPAREN] = PascalTextAttributeKeys.PARENTHESES.key

            keys1[PascalTokenType.LBRACE] = PascalTextAttributeKeys.BRACES.key
            keys1[PascalTokenType.RBRACE] = PascalTextAttributeKeys.BRACES.key

            keys1[PascalTokenType.LBRACKET] = PascalTextAttributeKeys.BRACKETS.key
            keys1[PascalTokenType.RBRACKET] = PascalTextAttributeKeys.BRACKETS.key

            keys1[PascalTokenType.COMMA] = PascalTextAttributeKeys.COMMA.key
            keys1[PascalTokenType.DOT] = PascalTextAttributeKeys.DOT.key
            keys1[PascalTokenType.SEMICOLON] = PascalTextAttributeKeys.SEMICOLON.key

            keys1[PascalTokenType.COLON] = PascalTextAttributeKeys.COLON.key
            keys1[PascalTokenType.EQ] = PascalTextAttributeKeys.EQ.key
            keys1[PascalTokenType.ASSIGN] = PascalTextAttributeKeys.ASSIGN.key
        }
    }

    override fun getTokenHighlights(tokenType: IElementType?): Array<out TextAttributesKey> {
        return pack(keys1[tokenType])
    }

    override fun getHighlightingLexer(): Lexer {
        return PascalLexer()
    }
}

class PascalSyntaxHighlighterFactory : SyntaxHighlighterFactory() {
    override fun getSyntaxHighlighter(project: Project?, virtualFile: VirtualFile?): SyntaxHighlighter {
        return PascalSyntaxHighlighter()
    }
}
