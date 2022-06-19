package org.jetbrains.plugins.pascal.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.text.CharSequenceReader;
import org.jetbrains.plugins.pascal.lang.PascalTokenType;

import java.io.BufferedReader;

import static com.intellij.psi.TokenType.BAD_CHARACTER;
import static com.intellij.psi.TokenType.WHITE_SPACE;
%%
%{
  public _PascalLexer() {
        this((java.io.Reader)null);
  }

  private static enum MultilineCommentType {
      PARENTHESES("(*", "*)"),
      BRACES("{", "}");

      private final String prefix;
      private final String postfix;

      private MultilineCommentType(String prefix, String postfix) {
          this.prefix = prefix;
          this.postfix = postfix;
      }
  }

  private boolean isEndOfComment(MultilineCommentType commentType) {
      switch (commentType) {
          case PARENTHESES:
            if (zzMarkedPos > zzBuffer.length() - 2) {
                return false;
            }
            return commentType.postfix.equals(zzBuffer.subSequence(zzMarkedPos, zzMarkedPos + 2).toString());
          case BRACES:
            return commentType.postfix.equals(String.valueOf(zzBuffer.charAt(zzMarkedPos)));
      }
      throw new IllegalStateException("Unknown multiline comment type " + commentType.name());
  }

  private static IElementType getMultulineCommentElementType(MultilineCommentType commentType) {
      switch (commentType) {
          case PARENTHESES:
              return PascalTokenType.Companion.getPARENTHESES_COMMENT();
          case BRACES:
              return PascalTokenType.Companion.getBRACES_COMMENT();
      }
      throw new IllegalStateException("Unknown multiline comment type " + commentType.name());
  }

  private IElementType parseOneLineComment() throws java.io.IOException {
      CharSequence remainingBuffer = zzBuffer.subSequence(zzMarkedPos, zzBuffer.length());
      try (BufferedReader reader = new BufferedReader(new CharSequenceReader(remainingBuffer))) {
          String line = reader.readLine();
          for (int i = 0; i < line.length(); i++) {
              ++zzMarkedPos;
          }
      }
      return PascalTokenType.Companion.getONE_LINE_COMMENT();
  }

  private IElementType parseMutltilineComment(MultilineCommentType commentType) {
    int postfixLen = commentType.postfix.length();
    while (zzMarkedPos <= zzBuffer.length() - postfixLen) {
      if (isEndOfComment(commentType)) {
        zzMarkedPos += postfixLen;
        return getMultulineCommentElementType(commentType);
      }
      ++zzMarkedPos;
    }
    return getMultulineCommentElementType(commentType);
  }
%}

%public
%class _PascalLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode


DEC_DIGIT = [0-9]
UNSIGNED_INTEGER = {DEC_DIGIT}+
UNSIGNED_FP_NUMBER = {DEC_DIGIT}+ "." {DEC_DIGIT}+

LINE_WS = [\ \t\f]+
EOL = "\r"|"\n"|"\r\n"
WHITE_SPACE=({LINE_WS}|{EOL})+

ALPHA_NUM = [a-zA-Z0-9_]
ID = [a-zA-Z] {ALPHA_NUM}*

STRING=(\"([^\"\r\n\\]|\\.)*\")
%%

"(*"             { return parseMutltilineComment(MultilineCommentType.PARENTHESES); }
"{"              { return parseMutltilineComment(MultilineCommentType.BRACES); }
"//"              { return parseOneLineComment(); }

{WHITE_SPACE}    { return WHITE_SPACE; }
{STRING}         { return PascalTokenType.Companion.getSTRING(); }
{UNSIGNED_INTEGER}    { return PascalTokenType.Companion.getUNSIGNED_INT(); }
{UNSIGNED_FP_NUMBER}    { return PascalTokenType.Companion.getUNSIGNED_FP_NUMBER(); }

"program"        { return PascalTokenType.Companion.getPROGRAM(); }

"uses"           { return PascalTokenType.Companion.getUSES(); }

"const"           { return PascalTokenType.Companion.getCONST(); }

"function"       { return PascalTokenType.Companion.getFUNCTION(); }
"procedure"      { return PascalTokenType.Companion.getPROCEDURE(); }

"var"      { return PascalTokenType.Companion.getVAR(); }

"do"      { return PascalTokenType.Companion.getDO(); }

"of"      { return PascalTokenType.Companion.getOF(); }

"array"      { return PascalTokenType.Companion.getARRAY(); }

"if"      { return PascalTokenType.Companion.getIF(); }
"then"      { return PascalTokenType.Companion.getTHEN(); }
"else"      { return PascalTokenType.Companion.getELSE(); }
"for"      { return PascalTokenType.Companion.getFOR(); }
"to"      { return PascalTokenType.Companion.getTO(); }
"downto"      { return PascalTokenType.Companion.getDOWNTO(); }
"until"      { return PascalTokenType.Companion.getUNTIL(); }
"while"      { return PascalTokenType.Companion.getWHILE(); }
"repeat"      { return PascalTokenType.Companion.getREPEAT(); }

"begin"      { return PascalTokenType.Companion.getBEGIN(); }
"end"      { return PascalTokenType.Companion.getEND(); }

"+"      { return PascalTokenType.Companion.getPLUS(); }
"-"      { return PascalTokenType.Companion.getMINUS(); }
"*"      { return PascalTokenType.Companion.getMULT(); }
"/"      { return PascalTokenType.Companion.getDIVIDE(); }

"div"      { return PascalTokenType.Companion.getDIV(); }
"mod"      { return PascalTokenType.Companion.getMOD(); }

"not"      { return PascalTokenType.Companion.getNOT(); }
"and"      { return PascalTokenType.Companion.getAND(); }
"or"      { return PascalTokenType.Companion.getOR(); }
"xor"      { return PascalTokenType.Companion.getXOR(); }

"shl"      { return PascalTokenType.Companion.getSHL(); }
"shr"      { return PascalTokenType.Companion.getSHR(); }

"in"      { return PascalTokenType.Companion.getIN(); }

"True"      { return PascalTokenType.Companion.getTRUE(); }
"False"      { return PascalTokenType.Companion.getFALSE(); }

"Nil"      { return PascalTokenType.Companion.getNIL(); }

{ID}             { return PascalTokenType.Companion.getID(); }

";"           { return PascalTokenType.Companion.getSEMICOLON(); }
":"           { return PascalTokenType.Companion.getCOLON(); }
".."           { return PascalTokenType.Companion.getDOUBLE_DOT(); }
"."           { return PascalTokenType.Companion.getDOT(); }
"["           { return PascalTokenType.Companion.getLBRACKET(); }
"]"           { return PascalTokenType.Companion.getRBRACKET(); }
"("           { return PascalTokenType.Companion.getLPAREN(); }
")"           { return PascalTokenType.Companion.getRPAREN(); }
"{"           { return PascalTokenType.Companion.getLBRACE(); }
"}"           { return PascalTokenType.Companion.getRBRACE(); }
","           { return PascalTokenType.Companion.getCOMMA(); }
"<"           { return PascalTokenType.Companion.getLT(); }
"<="           { return PascalTokenType.Companion.getLEQ(); }
">"           { return PascalTokenType.Companion.getGT(); }
">="           { return PascalTokenType.Companion.getGEQ(); }
"="           { return PascalTokenType.Companion.getEQ(); }
"<>"           { return PascalTokenType.Companion.getNEQ(); }
":="          { return PascalTokenType.Companion.getASSIGN(); }

[^] { return BAD_CHARACTER; }
