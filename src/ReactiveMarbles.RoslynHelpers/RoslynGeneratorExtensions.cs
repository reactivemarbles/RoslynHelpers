// Copyright (c) 2019-2021 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static ReactiveMarbles.RoslynHelpers.SyntaxFactoryHelpers;

namespace ReactiveMarbles.RoslynHelpers
{
    /// <summary>
    /// A common set of helper generator methods.
    /// </summary>
    public static class RoslynGeneratorExtensions
    {
        /// <summary>
        /// Generates a argument list for a single parameter.
        /// </summary>
        /// <param name="parameter">The parameter to generate the argument list for.</param>
        /// <returns>The argument list.</returns>
        public static IReadOnlyCollection<ArgumentSyntax> GenerateArgumentList(this IParameterSymbol parameter)
        {
            if (parameter is null)
            {
                throw new ArgumentNullException(nameof(parameter));
            }

            return new[] { Argument(parameter.Name.GetKeywordSafeName()) };
        }

        /// <summary>
        /// Generates a argument list for a tuple parameter.
        /// </summary>
        /// <param name="parameters">The parameters to generate the argument list for.</param>
        /// <returns>The argument list.</returns>
        public static IReadOnlyCollection<ArgumentSyntax> GenerateTupleArgumentList(this IEnumerable<IParameterSymbol> parameters) => new[] { Argument(TupleExpression(parameters.Select(x => Argument(x.Name.GetKeywordSafeName())).ToList())) };

        /// <summary>
        /// Generates a tuple type from specified type descriptors.
        /// </summary>
        /// <param name="typeDescriptors">The type descriptors.</param>
        /// <returns>The type syntax.</returns>
        public static TypeSyntax GenerateTupleType(this IReadOnlyCollection<(ITypeSymbol Type, string Name)> typeDescriptors)
        {
            if (typeDescriptors is null)
            {
                throw new ArgumentNullException(nameof(typeDescriptors));
            }

            var tupleTypes = new List<TupleElementSyntax>(typeDescriptors.Count);

            foreach (var typeDescriptor in typeDescriptors)
            {
                var typeName = typeDescriptor.Type.GenerateFullGenericName();
                var name = typeDescriptor.Name.GetKeywordSafeName();

                tupleTypes.Add(TupleElement(typeName, name));
            }

            return TupleType(tupleTypes);
        }

        /// <summary>
        /// Generates a IObservable argument list from the specified method.
        /// </summary>
        /// <param name="method">The method to generate for.</param>
        /// <returns>The argument list.</returns>
        public static TypeArgumentListSyntax GenerateObservableTypeArguments(this IMethodSymbol method)
        {
            if (method is null)
            {
                throw new ArgumentNullException(nameof(method));
            }

            // If we have no parameters, use the Unit type, if only one use the type directly, otherwise use a value tuple.
            if (method.Parameters.Length == 0)
            {
                return TypeArgumentList(new[] { IdentifierName(RoslynCommonHelpers.ObservableUnitName) });
            }
            else if (method.Parameters.Length == 1)
            {
                return TypeArgumentList(new[] { IdentifierName(method.Parameters[0].Type.GenerateFullGenericName()) });
            }
            else
            {
                return TypeArgumentList(new[] { TupleType(method.Parameters.Select(x => TupleElement(x.Type.GenerateFullGenericName(), x.Name)).ToList()) });
            }
        }

        /// <summary>
        /// Gets the members of the specified type.
        /// </summary>
        /// <typeparam name="T">The type of event symbol.</typeparam>
        /// <param name="symbol">The symbol to get members from.</param>
        /// <returns>The list of member target symbols.</returns>
        public static IEnumerable<T> GetMembers<T>(this INamedTypeSymbol symbol)
            where T : ISymbol
        {
            if (symbol is null)
            {
                throw new ArgumentNullException(nameof(symbol));
            }

            var members = symbol.GetMembers();
            for (var i = 0; i < members.Length; ++i)
            {
                var member = members[i];

                if (member is not T targetSymbol)
                {
                    continue;
                }

                yield return targetSymbol;
            }
        }

        /// <summary>
        /// Gets the events contained on the type.
        /// </summary>
        /// <param name="item">The items to include.</param>
        /// <param name="staticEvents">If the events should include static ones.</param>
        /// <param name="includeInherited">If the event sshould include inherited ones.</param>
        /// <returns>A collection of events contained on the type.</returns>
        public static IEnumerable<IEventSymbol> GetEvents(this INamedTypeSymbol item, bool staticEvents = false, bool includeInherited = true)
        {
            var baseClassWithEvents = includeInherited ? item.GetBasesWithCondition(RoslynCommonHelpers.HasEvents) : Array.Empty<INamedTypeSymbol>();

            var itemsToProcess = new Stack<INamedTypeSymbol>(new[] { item }.Concat(baseClassWithEvents));

            var processedItems = new HashSet<string>();
            while (itemsToProcess.Count != 0)
            {
                var namedType = itemsToProcess.Pop();

                var members = namedType.GetMembers();

                for (var memberIndex = 0; memberIndex < members.Length; memberIndex++)
                {
                    var member = members[memberIndex];

                    if (member is not IEventSymbol eventSymbol)
                    {
                        continue;
                    }

                    if (processedItems.Contains(member.Name))
                    {
                        continue;
                    }

                    processedItems.Add(member.Name);

                    if (!staticEvents && eventSymbol.IsStatic)
                    {
                        continue;
                    }

                    if (staticEvents && !eventSymbol.IsStatic)
                    {
                        continue;
                    }

                    if (eventSymbol.DeclaredAccessibility != Accessibility.Public)
                    {
                        continue;
                    }

                    var invokeMethod = ((INamedTypeSymbol)eventSymbol.OriginalDefinition.Type).DelegateInvokeMethod;

                    if (invokeMethod is null)
                    {
                        continue;
                    }

                    if (invokeMethod.ReturnType.SpecialType != SpecialType.System_Void)
                    {
                        continue;
                    }

                    yield return eventSymbol;
                }
            }
        }

        /// <summary>
        /// Generates a IObservable from the argument list.
        /// </summary>
        /// <param name="argumentList">The argument list.</param>
        /// <returns>The type.</returns>
        public static TypeSyntax GenerateObservableType(this TypeArgumentListSyntax argumentList) =>
            QualifiedName(IdentifierName("global::System"), GenericName("IObservable").WithTypeArgumentList(argumentList));

        /// <summary>
        /// Generates an observable from the specifiec type.
        /// </summary>
        /// <param name="argumentList">The argument type.</param>
        /// <returns>The type.</returns>
        public static TypeSyntax GenerateObservableType(this TypeSyntax argumentList) => QualifiedName(IdentifierName("global::System"), GenericName("IObservable", new[] { argumentList }));

        /// <summary>
        /// Generates parameters from a method symbol.
        /// </summary>
        /// <param name="method">The method to generate from.</param>
        /// <returns>The parameters.</returns>
        public static IReadOnlyList<ParameterSyntax> GenerateMethodParameters(this IMethodSymbol method)
        {
            if (method is null)
            {
                throw new ArgumentNullException(nameof(method));
            }

            if (method.Parameters.Length == 0)
            {
                return Array.Empty<ParameterSyntax>();
            }

            return method.Parameters.Select(
                        x => Parameter(x.Type.GenerateFullGenericName(), x.Name.GetKeywordSafeName())).ToList();
        }

        /// <summary>
        /// Gets all the base types and also the type being passed in.
        /// </summary>
        /// <typeparam name="T">The type of symbol.</typeparam>
        /// <param name="type">The type.</param>
        /// <returns>The type then base symbols.</returns>
        public static IEnumerable<T> GetBaseTypesAndThis<T>(this T type)
            where T : ITypeSymbol
        {
            var current = type;
            while (current is not null)
            {
                yield return current;
                current = (T?)current.BaseType;
            }
        }

        /// <summary>
        /// Gets all the base types and also the type being passed in that match the criteria.
        /// </summary>
        /// <typeparam name="T">The type of symbol.</typeparam>
        /// <param name="type">The type.</param>
        /// <param name="condition">The condition to match.</param>
        /// <returns>The type then base symbols.</returns>
        public static IEnumerable<T> GetBaseTypesAndThis<T>(this T type, Func<T?, bool> condition)
            where T : ITypeSymbol
        {
            if (condition is null)
            {
                throw new ArgumentNullException(nameof(condition));
            }

            var current = type;
            while (current is not null)
            {
                if (condition.Invoke(current))
                {
                    yield return current;
                }

                current = (T?)current.BaseType;
            }
        }

        /// <summary>
        /// Gets the type parameters from types.
        /// </summary>
        /// <param name="types">The types to extract.</param>
        /// <returns>The type parameters.</returns>
        public static IReadOnlyList<TypeParameterSyntax> ToTypeParameters(this IEnumerable<INamedTypeSymbol> types) => types.Select(x => TypeParameter(x.GenerateFullGenericName())).ToList();

        /// <summary>
        /// Gets a string form of the type and generic arguments for a type.
        /// </summary>
        /// <param name="currentType">The type to generate the arguments for.</param>
        /// <returns>A type descriptor including the generic arguments.</returns>
        public static string GenerateFullGenericName(this ITypeSymbol currentType)
        {
            if (currentType is null)
            {
                throw new ArgumentNullException(nameof(currentType));
            }

            return currentType.ToDisplayString(RoslynCommonHelpers.TypeFormat);
        }

        /// <summary>
        /// Gets base classes that match the conditions.
        /// </summary>
        /// <typeparam name="T">The type of symbol.</typeparam>
        /// <param name="symbol">The symbol.</param>
        /// <param name="condition">The condition to match.</param>
        /// <returns>The base types.</returns>
        public static IEnumerable<T> GetBasesWithCondition<T>(this T symbol, Func<T, bool> condition)
            where T : INamedTypeSymbol
        {
            if (condition is null)
            {
                throw new ArgumentNullException(nameof(condition));
            }

            var current = (T?)symbol.BaseType;

            while (current is not null)
            {
                if (condition.Invoke(current))
                {
                    yield return current;
                }

                current = (T?)current.BaseType;
            }
        }

        /// <summary>
        /// Gets the named symbol in arity format.
        /// </summary>
        /// <param name="namedTypeSymbol">The symbol to convert.</param>
        /// <returns>The formatted string.</returns>
        public static string GetArityDisplayName(this ITypeSymbol namedTypeSymbol)
        {
            if (namedTypeSymbol is null)
            {
                throw new ArgumentNullException(nameof(namedTypeSymbol));
            }

            var items = new Stack<ITypeSymbol>();

            var current = namedTypeSymbol;

            while (current is not null)
            {
                items.Push(current);
                current = current.ContainingType;
            }

            var stringBuilder = new StringBuilder();
            var i = 0;
            while (items.Count != 0)
            {
                var item = items.Pop();

                if (i != 0)
                {
                    stringBuilder.Append('.');
                }
                else
                {
                    stringBuilder.Append(namedTypeSymbol.ContainingNamespace.ToDisplayString()).Append('.');
                }

                stringBuilder.Append(item.Name);

                if (item is INamedTypeSymbol aritySymbol && aritySymbol.Arity > 0)
                {
                    stringBuilder.Append('`')
                        .Append(aritySymbol.Arity.ToString(CultureInfo.InvariantCulture));
                }

                i++;
            }

            return stringBuilder.ToString();
        }

        private static string GetKeywordSafeName(this string name) => TypesMetadata.CSharpKeywords.Contains(name) ? '@' + name : name;
    }
}
