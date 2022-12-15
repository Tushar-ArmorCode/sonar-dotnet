/*
 * SonarSource :: .NET :: Shared library
 * Copyright (C) 2014-2022 SonarSource SA
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
package org.sonar.plugins.vbnet;

import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonarsource.dotnet.shared.plugins.AbstractFileCacheSensor;
import org.sonarsource.dotnet.shared.plugins.HashProvider;

public class VbNetFileCacheSensor extends AbstractFileCacheSensor {
  public VbNetFileCacheSensor(VbNet vbNet, HashProvider hashProvider) {
    super(vbNet, hashProvider);
  }

  @Override
  public void describe(SensorDescriptor descriptor) {
    descriptor.name("VbNet file caching sensor");
    descriptor.onlyOnLanguage(VbNetPlugin.LANGUAGE_KEY);
  }
}
