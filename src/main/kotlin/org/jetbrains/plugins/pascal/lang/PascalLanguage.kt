package org.jetbrains.plugins.pascal.lang

import com.intellij.lang.Language
import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.vfs.VirtualFile
import java.nio.charset.StandardCharsets

object PascalLanguage : Language("Pascal")

object PascalFileType : LanguageFileType(PascalLanguage) {
    override fun getDisplayName() = "Pascal"
    override fun getName() = "Pascal"
    override fun getDescription() = "Pascal"
    override fun getDefaultExtension() = "pas"
    override fun getIcon() = null
    override fun getCharset(file: VirtualFile, content: ByteArray): String = StandardCharsets.UTF_8.name()
}
