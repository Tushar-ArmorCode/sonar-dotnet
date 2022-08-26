﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2022 SonarSource SA
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

using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using SonarAnalyzer.SymbolicExecution.Constraints;
using SonarAnalyzer.SymbolicExecution.Roslyn.Checks;
using StyleCop.Analyzers.Lightup;

namespace SonarAnalyzer.SymbolicExecution.Roslyn.OperationProcessors
{
    internal static class Pattern
    {
        public static ProgramState Process(SymbolicContext context, IIsPatternOperationWrapper isPattern) =>
            (BoolContraintFromConstant(context.State, isPattern) ?? BoolConstraintFromPattern(context.State, isPattern)) is { } constraint
                ? context.SetOperationConstraint(constraint)
                : context.State;

        public static ProgramState Process(SymbolicContext context, IRecursivePatternOperationWrapper recursive) =>
            ProcessDeclaration(context, recursive.DeclaredSymbol, true);

        public static ProgramState Process(SymbolicContext context, IDeclarationPatternOperationWrapper declaration) =>
            ProcessDeclaration(context, declaration.DeclaredSymbol, !declaration.MatchesNull);  // "... is var ..." should not set NotNull

        private static BoolConstraint BoolContraintFromConstant(ProgramState state, IIsPatternOperationWrapper isPattern) =>
            state[isPattern.Value] is { } value
            && isPattern.Pattern.WrappedOperation.Kind == OperationKindEx.ConstantPattern
            && ConstantCheck.ConstraintFromValue(IConstantPatternOperationWrapper.FromOperation(isPattern.Pattern.WrappedOperation).Value.ConstantValue.Value) is BoolConstraint boolPattern
            && PatternBoolConstraint(value, boolPattern) is { } newConstraint
                ? newConstraint
                : null;

        private static SymbolicConstraint BoolConstraintFromPattern(ProgramState state, IIsPatternOperationWrapper isPattern) =>
            state[isPattern.Value] is { } value
            && value.Constraint<ObjectConstraint>() is { } valueConstraint
            && BoolConstraintFromPattern(state, valueConstraint, isPattern.Pattern) is { } newConstraint
                ? newConstraint
                : null;

        private static SymbolicConstraint BoolConstraintFromPattern(ProgramState state, ObjectConstraint valueConstraint, IPatternOperationWrapper pattern)
        {
            return pattern.WrappedOperation.Kind switch
            {
                OperationKindEx.ConstantPattern when
                    As(IConstantPatternOperationWrapper.FromOperation) is var constantPattern
                    && state[constantPattern.Value]?.HasConstraint(ObjectConstraint.Null) is true =>
                        BoolConstraint.From(valueConstraint == ObjectConstraint.Null),
                OperationKindEx.RecursivePattern when As(IRecursivePatternOperationWrapper.FromOperation) is var recursivePattern =>
                    recursivePattern switch
                    {
                        _ when valueConstraint == ObjectConstraint.Null => BoolConstraint.False,
                        {
                            PropertySubpatterns.Length: 0,
                            DeconstructionSubpatterns.Length: 0,
                        } => BoolConstraint.True,
                        {
                            PropertySubpatterns: var propertySubPatterns,
                            DeconstructionSubpatterns: var deconstructSubpatterns,
                        } when propertySubPatterns // check if all sub pattern are always matching.
                                   .Select(x => IPropertySubpatternOperationWrapper.FromOperation(x).Pattern)
                                   .Concat(deconstructSubpatterns.Select(x => IPatternOperationWrapper.FromOperation(x)))
                                   .All(x => x is { WrappedOperation.Kind: OperationKindEx.DiscardPattern }
                                        || (x.WrappedOperation.Kind == OperationKindEx.DeclarationPattern
                                            && IDeclarationPatternOperationWrapper.FromOperation(x.WrappedOperation).MatchesNull)) =>
                                BoolConstraint.True,
                        _ => null,
                    },
                OperationKindEx.DeclarationPattern when As(IDeclarationPatternOperationWrapper.FromOperation) is var declarationPattern =>
                        declarationPattern switch
                        {
                            { MatchesNull: true } => BoolConstraint.True,
                            { MatchesNull: false } when valueConstraint == ObjectConstraint.Null => BoolConstraint.False,
                            var notNull when IsTypeAssignableTo(notNull.InputType, notNull.NarrowedType) => BoolConstraint.From(valueConstraint == ObjectConstraint.NotNull),
                            _ => null,
                        },
                OperationKindEx.TypePattern when
                    As(ITypePatternOperationWrapper.FromOperation) is var typePattern
                    && IsTypeAssignableTo(typePattern.InputType, typePattern.NarrowedType) =>
                        BoolConstraint.From(valueConstraint == ObjectConstraint.NotNull),
                OperationKindEx.NegatedPattern when
                    As(INegatedPatternOperationWrapper.FromOperation) is var negatedPattern =>
                        BoolConstraintFromPattern(state, valueConstraint, negatedPattern.Pattern)?.Opposite,
                OperationKindEx.DiscardPattern => BoolConstraint.True,
                OperationKindEx.BinaryPattern when
                    As(IBinaryPatternOperationWrapper.FromOperation) is var binaryPattern =>
                        BoolConstraintFromBinarryPattern(state, valueConstraint, binaryPattern),
                _ => null,
            };
        public static ProgramState Process(SymbolicContext context, IIsPatternOperationWrapper isPattern) =>
            context.State[isPattern.Value] is { } value
            && isPattern.Pattern.WrappedOperation.Kind == OperationKindEx.ConstantPattern
            && ConstantCheck.ConstraintFromValue(IConstantPatternOperationWrapper.FromOperation(isPattern.Pattern.WrappedOperation).Value.ConstantValue.Value) is BoolConstraint boolPattern
            && PatternBoolConstraint(value, boolPattern) is { } newConstraint
                ? context.SetOperationConstraint(newConstraint)
                : context.State;

            T As<T>(Func<IOperation, T> fromOperation) =>
                 fromOperation(pattern.WrappedOperation);
        }

        private static BoolConstraint BoolConstraintFromBinarryPattern(ProgramState state, ObjectConstraint valueConstraint, IBinaryPatternOperationWrapper binaryPattern)
        {
            var left = BoolConstraintFromPattern(state, valueConstraint, binaryPattern.LeftPattern);
            var right = BoolConstraintFromPattern(state, valueConstraint, binaryPattern.RightPattern);
            return binaryPattern.OperatorKind switch
            {
                BinaryOperatorKind.And when left != null && right != null => BoolConstraint.From(left == BoolConstraint.True && right == BoolConstraint.True),
                BinaryOperatorKind.Or when left != null || right != null => BoolConstraint.From(left == BoolConstraint.True || right == BoolConstraint.True),
                _ => null,
            };
        }

        private static bool IsTypeAssignableTo(ITypeSymbol type, ITypeSymbol assignableTo) =>
            type.Equals(assignableTo); // Compilation.ClassifyConversion should be used for better results (Note: for cfg IOperation.SemanticModel is null)

        public static ProgramState Process(SymbolicContext context, IRecursivePatternOperationWrapper recursive) =>
            ProcessDeclaration(context, recursive.DeclaredSymbol, true);

        public static ProgramState Process(SymbolicContext context, IDeclarationPatternOperationWrapper declaration) =>
            ProcessDeclaration(context, declaration.DeclaredSymbol, !declaration.MatchesNull);  // "... is var ..." should not set NotNull

        public static ProgramState LearnBranchingConstraint(ProgramState state, IIsPatternOperationWrapper isPattern, bool useOpposite) =>
            state.ResolveCapture(isPattern.Value).TrackedSymbol() is { } testedSymbol
            && LearnBranchingConstraint(state, isPattern.Pattern, useOpposite) is { } constraint
                ? state.SetSymbolConstraint(testedSymbol, constraint)
                : state;

        private static SymbolicConstraint LearnBranchingConstraint(ProgramState state, IPatternOperationWrapper pattern, bool useOpposite)
        {
            return pattern.WrappedOperation.Kind switch
            {
                OperationKindEx.ConstantPattern => ConstraintFromConstantPattern(state, As(IConstantPatternOperationWrapper.FromOperation), useOpposite),
                OperationKindEx.NegatedPattern => LearnBranchingConstraint(state, As(INegatedPatternOperationWrapper.FromOperation).Pattern, !useOpposite),
                OperationKindEx.TypePattern => ObjectConstraint.NotNull.ApplyOpposite(useOpposite),
                _ => null
            };

            T As<T>(Func<IOperation, T> fromOperation) =>
                fromOperation(pattern.WrappedOperation);
        }

        private static SymbolicConstraint ConstraintFromConstantPattern(ProgramState state, IConstantPatternOperationWrapper constant, bool useOpposite)
        {
            if (state[constant.Value] is { } value)
            {
                if (value.Constraint<BoolConstraint>() is { } boolConstraint && !useOpposite)   // Cannot use opposite on booleans. If it is not "true", it could be null, false or any other type
                {
                    return boolConstraint;
                }
                else if (value.Constraint<ObjectConstraint>() is { } objectConstraint)
                {
                    return objectConstraint.ApplyOpposite(useOpposite);
                }
            }
            return null;
        }

        private static BoolConstraint PatternBoolConstraint(SymbolicValue value, BoolConstraint pattern) =>
            value.HasConstraint<BoolConstraint>()
                ? BoolConstraint.From(value.HasConstraint(pattern))
                : null; // We cannot take conclusive decision

        private static ProgramState ProcessDeclaration(SymbolicContext context, ISymbol declaredSymbol, bool setNotNull)
        {
            if (declaredSymbol == null)
            {
                return context.State;
            }
            else
            {
                var state = context.Operation.Parent.AsIsPattern() is { } parentIsPattern && parentIsPattern.Value.TrackedSymbol() is { } sourceSymbol
                    ? context.State.SetSymbolValue(declaredSymbol, context.State[sourceSymbol])  // ToDo: MMF-2563 should define relation between tested and declared symbol
                    : context.State;
                return setNotNull
                    ? state.SetSymbolConstraint(declaredSymbol, ObjectConstraint.NotNull)
                    : state;
            }
        }
    }
}
