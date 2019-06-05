/*
 * SonarSource :: .NET :: Shared library
 * Copyright (C) 2014-2019 SonarSource SA
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
package org.sonarsource.dotnet.shared.plugins;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.sonar.api.batch.InstantiationStrategy;
import org.sonar.api.batch.ScannerSide;
import org.sonar.api.config.Configuration;
import org.sonar.api.utils.log.Logger;
import org.sonar.api.utils.log.Loggers;

import static java.util.Arrays.asList;
import static org.sonarsource.dotnet.shared.plugins.AbstractPropertyDefinitions.getAnalyzerWorkDirProperty;
import static org.sonarsource.dotnet.shared.plugins.AbstractPropertyDefinitions.getRoslynJsonReportPathProperty;

@ScannerSide
@InstantiationStrategy(InstantiationStrategy.PER_BATCH)
public abstract class AbstractSolutionConfiguration {
  private static final Logger LOG = Loggers.get(AbstractSolutionConfiguration.class);
  private static final String MSG_SUFFIX = "Analyzer results won't be loaded from this directory.";
  private static final String PROP_PREFIX = "sonar.";

  private final Configuration configuration;
  private final String languageKey;

  public AbstractSolutionConfiguration(Configuration configuration, String languageKey) {
    this.configuration = configuration;
    this.languageKey = languageKey;
  }

  public boolean analyzeGeneratedCode() {
    return configuration.getBoolean(AbstractPropertyDefinitions.getAnalyzeGeneratedCode(languageKey)).orElse(false);
  }
}
