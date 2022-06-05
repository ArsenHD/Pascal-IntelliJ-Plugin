package org.jetbrains.plugins.pascal.lang

import com.intellij.testFramework.ParsingTestCase

abstract class PascalParsingTestBase(baseDir: String) : ParsingTestCase(baseDir, "pas", true, PascalParserDefinition()) {
  override fun getTestDataPath() = "src/test/data"
}

class PascalParsingTest : PascalParsingTestBase("parser") {

  fun testCompoundStatement() = doTest(true)
  fun testConstantDeclaration() = doTest(true)
  fun testForLoop() = doTest(true)
  fun testFunctionDeclaration() = doTest(true)
  fun testProcedureDeclaration() = doTest(true)
  fun testPrecedence() = doTest(true)
  fun testRepeatUntil() = doTest(true)
  fun testUsesClause() = doTest(true)
  fun testVariableDeclaration() = doTest(true)
  fun testWhileLoop() = doTest(true)
}
