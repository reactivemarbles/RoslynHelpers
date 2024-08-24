// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using static ReactiveMarbles.RoslynHelpers.Helpers.SyntaxFactoryHelpers;

namespace ReactiveMarbles.RoslynHelpers.Extensions;

/// <summary>
/// Extension methods for the <see cref="SyntaxToken"/> type.
/// </summary>
public static class SyntaxTokenExtensions
{
    /// <summary>
    /// Deconstructs a <see cref="SyntaxToken"/> into its <see cref="SyntaxKind"/> value.
    /// </summary>
    /// <param name="syntaxToken">The input <see cref="SyntaxToken"/> value.</param>
    /// <param name="syntaxKind">The resulting <see cref="SyntaxKind"/> value for <paramref name="syntaxToken"/>.</param>
    public static void Deconstruct(this in SyntaxToken syntaxToken, out SyntaxKind syntaxKind) => syntaxKind = syntaxToken.Kind();

    /// <summary>
    /// Adds leading new lines trivia.
    /// </summary>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of lines.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SyntaxToken AddLeadingNewLines(this in SyntaxToken item, int number = 1)
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
    /// <param name="item">The item.</param>
    /// <param name="number">The number of lines.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SyntaxToken AddTrialingNewLines(this in SyntaxToken item, int number = 1)
    {
        if (number == 0)
        {
            return item;
        }

        var carriageReturnList = Enumerable.Repeat(CarriageReturnLineFeed, number);
        return item.WithTrailingTrivia(carriageReturnList);
    }

    /// <summary>
    /// Adds leading new lines and spaces.
    /// </summary>
    /// <param name="item">The item.</param>
    /// <param name="numberNewLines">The number of lines.</param>
    /// <param name="numberSpaces">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SyntaxToken AddLeadingNewLinesAndSpaces(this in SyntaxToken item, int numberNewLines = 1, int numberSpaces = 1)
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
    /// Adds leading new spaces trivia.
    /// </summary>
    /// <param name="item">The item.</param>
    /// <param name="number">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SyntaxToken AddLeadingSpaces(this in SyntaxToken item, int number = 1)
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
    /// <param name="item">The item.</param>
    /// <param name="number">The number of spaces.</param>
    /// <returns>The updated token with the trivia.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SyntaxToken AddTrialingSpaces(this in SyntaxToken item, int number = 1)
    {
        if (number == 0)
        {
            return item;
        }

        var leadingSpaces = Enumerable.Repeat(Space, number);
        return item.WithTrailingTrivia(leadingSpaces);
    }
}
