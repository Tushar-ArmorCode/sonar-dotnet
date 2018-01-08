/*
 * SonarSource :: .NET :: Shared library
 * Copyright (C) 2014-2018 SonarSource SA
 * mailto:info AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonarsource.dotnet.shared.plugins.protobuf;

import static org.sonarsource.dotnet.shared.plugins.SensorContextUtils.toTextRange;

import javax.annotation.CheckForNull;

import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.highlighting.NewHighlighting;
import org.sonar.api.batch.sensor.highlighting.TypeOfText;
import org.sonarsource.dotnet.protobuf.SonarAnalyzer;
import org.sonarsource.dotnet.protobuf.SonarAnalyzer.TokenTypeInfo;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

class HighlightImporter extends ProtobufImporter<SonarAnalyzer.TokenTypeInfo> {

  private final SensorContext context;
  private final Map<InputFile, HashSet<TokenTypeInfo.TokenInfo>> fileHighlights = new HashMap<>();

  HighlightImporter(SensorContext context) {
    super(SonarAnalyzer.TokenTypeInfo.parser(), context, SonarAnalyzer.TokenTypeInfo::getFilePath);
    this.context = context;
  }

  @Override
  void consumeFor(InputFile inputFile, TokenTypeInfo message) {
    for (SonarAnalyzer.TokenTypeInfo.TokenInfo tokenInfo : message.getTokenInfoList()) {
      fileHighlights
        .computeIfAbsent(inputFile, f -> new HashSet<>())
        .add(tokenInfo);
    }
  }

  @Override
  public void save() {
    for (Map.Entry<InputFile, HashSet<SonarAnalyzer.TokenTypeInfo.TokenInfo>> entry : fileHighlights.entrySet()) {
      NewHighlighting highlighting = context.newHighlighting().onFile(entry.getKey());

      boolean foundMappableHighlightings = false;
      for (SonarAnalyzer.TokenTypeInfo.TokenInfo message : entry.getValue()) {
        TypeOfText typeOfText = toType(message.getTokenType());
        if (typeOfText != null) {
          highlighting.highlight(toTextRange(entry.getKey(), message.getTextRange()), typeOfText);
          foundMappableHighlightings = true;
        }
      }
      if (foundMappableHighlightings) {
        highlighting.save();
      }
    }
  }

  @Override
  boolean isProcessed(InputFile inputFile) {
    // we aggregate all highlighting information, no need to process only the first protobuf file
    return false;
  }

  @CheckForNull
  private static TypeOfText toType(SonarAnalyzer.TokenType tokenType) {
    // Note:
    // TypeOfText.ANNOTATION -> like a type in C#, so received as DECLARATION_NAME
    // TypeOfText.STRUCTURED_COMMENT -> not colored differently in C#, so received as COMMENT
    // TypeOfText.PREPROCESS_DIRECTIVE -> received as KEYWORD

    switch (tokenType) {
      case NUMERIC_LITERAL:
        return TypeOfText.CONSTANT;

      case COMMENT:
        return TypeOfText.COMMENT;

      case KEYWORD:
        return TypeOfText.KEYWORD;

      case TYPE_NAME:
        return TypeOfText.KEYWORD_LIGHT;

      case STRING_LITERAL:
        return TypeOfText.STRING;

      case UNRECOGNIZED:
        // generated by protobuf
      case UNKNOWN:
      default:
        // do not color
        return null;
    }
  }
}
