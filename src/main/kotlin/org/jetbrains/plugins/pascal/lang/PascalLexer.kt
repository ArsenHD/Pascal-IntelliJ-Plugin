package org.jetbrains.plugins.pascal.lang

import com.intellij.lexer.FlexAdapter
import org.jetbrains.plugins.pascal.lexer._PascalLexer

class PascalLexer : FlexAdapter(_PascalLexer(null))
