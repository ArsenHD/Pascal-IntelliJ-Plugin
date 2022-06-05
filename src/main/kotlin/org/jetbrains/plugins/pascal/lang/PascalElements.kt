package org.jetbrains.plugins.pascal.lang

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.extapi.psi.PsiFileBase
import com.intellij.lang.ASTNode
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider
import com.intellij.psi.NavigatablePsiElement

interface PascalNavigatableElement : NavigatablePsiElement {
}


class PascalFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, PascalLanguage), PascalNavigatableElement {
    override fun getFileType(): FileType = PascalFileType
}

class PascalPsiElement(node: ASTNode) : ASTWrapperPsiElement(node) {
    override fun getContainingFile(): PascalFile? = super.getContainingFile() as? PascalFile
}
