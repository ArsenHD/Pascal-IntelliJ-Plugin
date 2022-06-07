package org.jetbrains.plugins.pascal.lang

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFileFactory
import com.intellij.psi.util.descendantsOfType

class PascalElementFactory(private val project: Project) {
	val factory = PsiFileFactory.getInstance(project)

    fun createFunctionIdentifier(name: String): PsiElement {
        return create<PascalFunction>("function $name(): A\nbegin\nend;")!!.nameIdentifier!!
    }

    fun createProcedureIdentifier(name: String): PsiElement {
        return create<PascalProcedure>("procedure $name()\nbegin\nend;")!!.nameIdentifier!!
    }

    fun createTypeRefIdentifier(name: String): PsiElement {
        return create<PascalTypeRef>(name)!!.nameIdentifier!!
    }

    fun createCallExpressionIdentifier(name: String): PsiElement {
        return create<PascalCallExpression>("$name()")!!.nameIdentifier!!
    }

    fun createVariableAccessIdentifier(name: String): PsiElement {
        return create<PascalVariableAccess>(name)!!.nameIdentifier!!
    }

    fun createVariableIdentifier(name: String): PsiElement {
        return create<PascalVariableDeclaration>("var $name: A")!!.nameIdentifier!!
    }

    fun createParameterIdentifier(name: String): PsiElement {
        return create<PascalParameter>("$name: A")!!.nameIdentifier!!
    }

	private inline fun <reified T : PsiElement> create(code: String): T? {
		return factory.createFileFromText("tmp.pas", PascalFileType, code).descendantsOfType<T>().firstOrNull()
	}
}
