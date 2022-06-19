package org.jetbrains.plugins.pascal.lang

import com.intellij.psi.stubs.PsiFileStub
import com.intellij.psi.tree.IStubFileElementType

class PascalStubFileElementType : IStubFileElementType<PsiFileStub<PascalFile>>(PascalLanguage) {
    override fun getStubVersion(): Int = 2
}
