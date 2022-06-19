package org.jetbrains.plugins.pascal.lang

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.extapi.psi.PsiFileBase
import com.intellij.lang.ASTNode
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider
import com.intellij.psi.NavigatablePsiElement
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiNameIdentifierOwner
import com.intellij.psi.PsiReference
import com.intellij.psi.PsiReferenceBase

interface PascalNavigatableElement : NavigatablePsiElement

class PascalFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, PascalLanguage), PascalNavigatableElement {
    override fun getFileType(): FileType = PascalFileType
}

open class PascalPsiElement(node: ASTNode) : ASTWrapperPsiElement(node) {
    override fun getContainingFile(): PascalFile? = super.getContainingFile() as? PascalFile
}

interface PascalNamedElement : PsiNameIdentifierOwner

abstract class PascalNamedPsiElement(node: ASTNode) : PascalPsiElement(node), PascalNamedElement {
    override fun getNameIdentifier(): PsiElement? = findChildByType(PascalTokenType.IDENTS)
    override fun getName(): String = nameIdentifier?.text.orEmpty()

    override fun getNavigationElement(): PsiElement = nameIdentifier ?: this
    override fun getTextOffset() = nameIdentifier?.textOffset ?: super.getTextOffset()
}

interface PascalDeclarationContainer : PsiElement {
    val callableDeclarations: List<PascalCallableDeclaration>
        get() = procedures + functions

    val procedures: List<PascalProcedure>
        get() = emptyList()

    val functions: List<PascalFunction>
        get() = emptyList()

    val variables: List<PascalVariableDeclaration>
}

class PascalBlock(node: ASTNode) : PascalPsiElement(node), PascalDeclarationContainer {
    private val declarationPart: PascalBlockDeclarationPart?
        get() = findChildByType(PascalElementType.BLOCK_DECLARATION_PART)

    override val functions: List<PascalFunction>
        get() = declarationPart?.functions ?: emptyList()

    override val procedures: List<PascalProcedure>
        get() = declarationPart?.procedures ?: emptyList()

    override val variables: List<PascalVariableDeclaration>
        get() = declarationPart?.variables ?: emptyList()

    val statementPart: PascalBlockStatementPart?
        get() = findChildByType(PascalElementType.BLOCK_STATEMENT_PART)
}

class PascalBlockDeclarationPart(node: ASTNode) : PascalPsiElement(node), PascalDeclarationContainer {
    override val procedures: List<PascalProcedure>
        get() = findChildrenByType(PascalElementType.PROCEDURE_DECLARATION)

    override val functions: List<PascalFunction>
        get() = findChildrenByType(PascalElementType.FUNCTION_DECLARATION)

    private val variableDeclarationParts: List<PascalVariableDeclarationPart>
        get() = findChildrenByType(PascalElementType.VARIABLE_DECLARATIONS)

    override val variables: List<PascalVariableDeclaration>
        get() = variableDeclarationParts.flatMap { it.variables }
}

class PascalVariableDeclarationPart(node: ASTNode) : PascalPsiElement(node) {
    val variables: List<PascalVariableDeclaration>
        get() = findChildrenByType(PascalElementType.VARIABLE_DECLARATION)
}

class PascalBlockStatementPart(node: ASTNode) : PascalPsiElement(node)

abstract class PascalCallableDeclaration(node: ASTNode) : PascalNamedPsiElement(node), PascalDeclarationContainer {
    private val parameterList: PascalParameterList?
        get() = findChildByType(PascalElementType.PARAMETER_LIST)

    override val variables: List<PascalVariableDeclaration>
        get() = parameters

    val parameters: List<PascalParameter>
        get() = parameterList?.parameters ?: emptyList()
}

class PascalProcedure(node: ASTNode) : PascalCallableDeclaration(node) {
    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createProcedureIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

class PascalFunction(node: ASTNode) : PascalCallableDeclaration(node) {
    val returnType: PascalType
        get() {
            val typeRef = findChildByType<PascalTypeRef>(PascalElementType.TYPE_REFERENCE)
            return typeRef!!.type!!
        }

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createFunctionIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

class PascalParameterList(node: ASTNode) : PascalPsiElement(node) {
    val parameters: List<PascalParameter>
        get() = findChildrenByType(PascalElementType.PARAMETER)
}

class PascalParameter(node: ASTNode) : PascalVariableDeclaration(node) {
    val typeRef: PascalTypeRef?
        get() = findChildByType(PascalElementType.TYPE_REFERENCE)

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createParameterIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

class PascalArrayDimensions(node: ASTNode) : PascalPsiElement(node) {
    val size: Int
        get() = ranges.size

    val ranges: List<PascalSubrangeType>
        get() = findChildrenByType(PascalElementType.SUBRANGE_TYPE)
}

class PascalSubrangeType(node: ASTNode) : PascalPsiElement(node) {
    val start: Int?
        get() = findChildrenByType<PsiElement>(PascalTokenType.UNSIGNED_INT)
            .getOrNull(0)
            ?.text?.trim()?.toInt()

    val end: Int?
        get() = findChildrenByType<PsiElement>(PascalTokenType.UNSIGNED_INT)
            .getOrNull(1)
            ?.text?.trim()?.toInt()
}

class PascalTypeRef(node: ASTNode) : PascalNamedPsiElement(node) {
    val isArrayType: Boolean
        get() = findChildByType<PsiElement>(PascalTokenType.ARRAY) != null

    val type: PascalType?
        get() {
            if (!isArrayType) {
                return PascalRegularType(name)
            }
            val dimensions = findChildByType<PascalArrayDimensions>(PascalElementType.ARRAY_DIMENSIONS) ?: return null
            val ranges = dimensions.ranges
            val elementType = findChildByType<PascalTypeRef>(PascalElementType.TYPE_REFERENCE) ?: return null
            return PascalArrayType(ranges, elementType)
        }

    override fun getNameIdentifier(): PsiElement? {
        return when {
            isArrayType -> this
            else -> super.getNameIdentifier()
        }
    }

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createTypeRefIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

interface PascalType

object PascalErrorType : PascalType

data class PascalArrayType(
    val ranges: List<PascalSubrangeType>,
    val elementType: PascalTypeRef
) : PascalType

open class PascalRegularType(val name: String) : PascalType {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is PascalRegularType) return false

        if (name != other.name) return false

        return true
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}

object PascalBooleanType : PascalType

object PascalStringType : PascalRegularType("String")

object PascalIntegerType : PascalRegularType("Integer")

object PascalNilType : PascalRegularType("Nil")

object PascalRealType : PascalRegularType("Real")

interface PascalExpression : PsiElement {
    val type: PascalType?
}

class PascalBinaryExpression(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    val left: PascalExpression
        get() = findChildrenByType<PascalExpression>(PascalElementType.EXPRESSIONS)[0]

    val right: PascalExpression
        get() = findChildrenByType<PascalExpression>(PascalElementType.EXPRESSIONS)[1]

    override val type: PascalType
        get() = if (left.type != right.type) PascalErrorType else left.type!!
}

class PascalTrueConstant(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalBooleanType
}

class PascalFalseConstant(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalBooleanType
}

class PascalUnsignedIntNumber(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalIntegerType
}

class PascalUnsignedFloatNumber(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalRealType
}

class PascalCharacterString(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalStringType
}

class PascalNilReference(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    override val type: PascalType
        get() = PascalNilType
}

class PascalPrefixExpression(node: ASTNode) : PascalPsiElement(node), PascalExpression {
    val operand: PascalExpression
        get() = findChildByType(PascalElementType.EXPRESSIONS)!!

    override val type: PascalType?
        get() = operand.type
}

class PascalArgumentList(node: ASTNode) : PascalPsiElement(node) {
    val arguments: List<PascalExpression>
        get() = findChildrenByType(PascalElementType.EXPRESSIONS)
}

class PascalCallExpression(node: ASTNode) : PascalNamedPsiElement(node), PascalExpression {
    override val type: PascalType?
        get() {
            val declaration = reference?.resolve() as? PascalFunction ?: return null
            return declaration.returnType
        }

    private val argumentList: PascalArgumentList
        get() = findChildByType(PascalElementType.ARGUMENT_LIST)!!

    val arguments: List<PascalExpression>
        get() = argumentList.arguments

    override fun getReference(): PsiReferenceBase<PascalCallExpression>? {
        val token = nameIdentifier ?: return null
        val rangeInElement = token.textRangeInParent

        return object : PsiReferenceBase<PascalCallExpression>(this, rangeInElement) {
            override fun resolve(): PsiElement? {
                var node: PsiElement? = element
                while (node != null) {
                    val declarations = (node as? PascalBlock)?.callableDeclarations ?: emptyList()
                    val candidates = declarations
                        .filter { name == it.name }
                        .filter { candidate -> element matches candidate }
                    if (candidates.isNotEmpty()) {
                        return candidates.first()
                    }
                    node = node.parent
                }
                return null
            }
        }
    }

    private infix fun PascalCallExpression.matches(declaration: PascalCallableDeclaration): Boolean {
        val argumentTypes = arguments.map { it.type }
        val parameterTypes = declaration.parameters.map { it.type }
        return argumentTypes == parameterTypes
    }

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createCallExpressionIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

class PascalForLoopControlVariable(node: ASTNode) : PascalVariableDeclaration(node) {
    override val type: PascalType? = null
}

class PascalForLoop(node: ASTNode) : PascalPsiElement(node), PascalDeclarationContainer {
    override val variables: List<PascalVariableDeclaration>
        get() = listOf(controlVariable)

    val controlVariable: PascalForLoopControlVariable
        get() = findChildByType(PascalElementType.CONTROL_VARIABLE)!!
}

open class PascalVariableDeclaration(node: ASTNode) : PascalNamedPsiElement(node) {
    open val type: PascalType?
        get() {
            val typeRef = findChildByType<PascalTypeRef>(PascalElementType.TYPE_REFERENCE)
            return typeRef?.type
        }

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createVariableIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}

class PascalVariableAccess(node: ASTNode) : PascalNamedPsiElement(node), PascalExpression {
    override val type: PascalType
        get() {
            val declaration = reference?.resolve() as PascalVariableDeclaration
            return declaration.type!!
        }

    override fun getReference(): PsiReference? {
        val token = nameIdentifier ?: return null
        val rangeInElement = token.textRangeInParent

        return object : PsiReferenceBase<PascalVariableAccess>(this, rangeInElement) {
            override fun resolve(): PsiElement? {
                var node: PsiElement? = element
                while (node != null) {
                    val declarations = (node as? PascalDeclarationContainer)?.variables ?: emptyList()
                    val candidates = declarations.filter { name == it.name }
                    if (candidates.isNotEmpty()) {
                        return candidates.first()
                    }
                    node = node.parent
                }
                return null
            }
        }
    }

    override fun setName(name: String): PsiElement {
        val newIdentifier = PascalElementFactory(project).createVariableAccessIdentifier(name)
        nameIdentifier!!.replace(newIdentifier)
        return this
    }
}
