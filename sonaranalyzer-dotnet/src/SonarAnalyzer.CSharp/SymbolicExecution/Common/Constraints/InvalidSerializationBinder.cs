﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2020 SonarSource SA
 * mailto: contact AT sonarsource DOT com
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

namespace SonarAnalyzer.SymbolicExecution.Common.Constraints
{
    internal sealed class InvalidSerializationBinder : SymbolicValueConstraint
    {
        internal static readonly InvalidSerializationBinder Invalid = new InvalidSerializationBinder();
        internal static readonly InvalidSerializationBinder Valid = new InvalidSerializationBinder();

        private InvalidSerializationBinder()
        {
        }

        public override SymbolicValueConstraint OppositeForLogicalNot =>
            this == Valid
                ? Invalid
                : Valid;

        public override string ToString() =>
            this == Valid
                ? "Valid"
                : "Invalid";
    }
}
