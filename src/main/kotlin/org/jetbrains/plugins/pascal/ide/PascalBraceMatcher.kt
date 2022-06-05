package org.jetbrains.plugins.pascal.ide

import com.intellij.codeInsight.highlighting.PairedBraceMatcherAdapter
import com.intellij.lang.BracePair
import com.intellij.lang.PairedBraceMatcher
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.pascal.lang.PascalLanguage
import org.jetbrains.plugins.pascal.lang.PascalTokenType

class PascalBraceMatcher: PairedBraceMatcherAdapter(object : PairedBraceMatcher{
	val PAIRS = arrayOf(
		BracePair(PascalTokenType.LPAREN, PascalTokenType.RPAREN, false),
		BracePair(PascalTokenType.LBRACE, PascalTokenType.RBRACE, false),
		BracePair(PascalTokenType.LBRACKET, PascalTokenType.RBRACKET, false),
	)

	override fun getPairs(): Array<BracePair> {
		return PAIRS
	}

	override fun isPairedBracesAllowedBeforeType(lbraceType: IElementType, contextType: IElementType?): Boolean {
		return true
	}

	override fun getCodeConstructStart(file: PsiFile?, openingBraceOffset: Int): Int {
		return openingBraceOffset
	}
}, PascalLanguage)
