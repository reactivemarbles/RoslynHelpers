// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using static ReactiveMarbles.RoslynHelpers.Helpers.SyntaxFactoryHelpers;

namespace ReactiveMarbles.RoslynHelpers.Extensions;

/// <summary>
/// Extension methods for the <see cref="SyntaxNode"/> type.
/// </summary>
public static class SyntaxNodeExtensions
{
    /// <summary>
    /// Checks whether a given <see cref="SyntaxNode"/> represents the first (partial) declaration of a given symbol.
    /// </summary>
    /// <param name="syntaxNode">The input <see cref="SyntaxNode"/> instance.</param>
    /// <param name="symbol">The target <see cref="ISymbol"/> instance to check the syntax declaration for.</param>
    /// <returns>Whether <paramref name="syntaxNode"/> is the first (partial) declaration for <paramref name="symbol"/>.</returns>
    /// <remarks>
    /// This extension can be used to avoid accidentally generating repeated members for types that have multiple partial declarations.
    /// In order to keep this check efficient and without the need to collect all items and build some sort of hashset from them to
    /// remove duplicates, each syntax node is symply compared against the available declaring syntax references for the target symbol.
    /// If the syntax node matches the first syntax reference for the symbol, it is kept, otherwise it is considered a duplicate.
    /// </remarks>
    public static bool IsFirstSyntaxDeclarationForSymbol(this SyntaxNode syntaxNode, ISymbol symbol) => symbol?.DeclaringSyntaxReferences is [SyntaxReference syntaxReference, ..] &&
            syntaxReference.SyntaxTree == syntaxNode?.SyntaxTree &&
            syntaxReference.Span == syntaxNode.Span;

    /// <summary>
    /// Adds leading new lines and spaces.
    /// </summary>
    /// <typeparam name="T">The type of syntax node.</typeparam>
    /// <param name="item">The item.</param>
    /// <param name="numberNewLines">The number of lines.</param>
    /// <param name="numberSpaces">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T AddLeadingNewLinesAndSpaces<T>(this T item, int numberNewLines = 1, int numberSpaces = 1)
        where T : SyntaxNode
    {
        if (numberNewLines == 0 && numberSpaces == 0)
        {
            return item;
        }

        var carriageReturnList = Enumerable.Repeat(CarriageReturnLineFeed, numberNewLines);
        var leadingSpaces = Enumerable.Repeat(Space, numberSpaces);

        return item.WithLeadingTrivia(carriageReturnList.Concat(leadingSpaces));
    }

    /// <summary>
    /// Adds leading new lines trivia.
    /// </summary>
    /// <typeparam name="T">The type of syntax node.</typeparam>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of lines.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T AddLeadingNewLines<T>(this T item, int number = 1)
        where T : SyntaxNode
    {
        if (number == 0)
        {
            return item;
        }

        var carriageReturnList = Enumerable.Repeat(CarriageReturnLineFeed, number);
        return item.WithLeadingTrivia(carriageReturnList);
    }

    /// <summary>
    /// Adds trailing new lines trivia.
    /// </summary>
    /// <typeparam name="T">The type of syntax node.</typeparam>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of lines.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T AddTrialingNewLines<T>(this T item, int number = 1)
        where T : SyntaxNode
    {
        if (number == 0)
        {
            return item;
        }

        var carriageReturnList = Enumerable.Repeat(CarriageReturnLineFeed, number);
        return item.WithTrailingTrivia(carriageReturnList);
    }

    /// <summary>
    /// Adds leading new spaces trivia.
    /// </summary>
    /// <typeparam name="T">The type of syntax node.</typeparam>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T AddLeadingSpaces<T>(this T item, int number = 1)
        where T : SyntaxNode
    {
        if (number == 0)
        {
            return item;
        }

        var leadingSpaces = Enumerable.Repeat(Space, number);
        return item.WithLeadingTrivia(leadingSpaces);
    }

    /// <summary>
    /// Adds trailing new spaces trivia.
    /// </summary>
    /// <typeparam name="T">The type of syntax node.</typeparam>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T AddTrialingSpaces<T>(this T item, int number = 1)
        where T : SyntaxNode
    {
        if (number == 0)
        {
            return item;
        }

        var leadingSpaces = Enumerable.Repeat(Space, number);
        return item.WithTrailingTrivia(leadingSpaces);
    }
}
