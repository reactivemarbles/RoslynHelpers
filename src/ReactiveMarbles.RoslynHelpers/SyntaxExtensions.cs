// Copyright (c) 2019-2021 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System.Linq;
using System.Runtime.CompilerServices;

using Microsoft.CodeAnalysis;

using static ReactiveMarbles.RoslynHelpers.SyntaxFactoryHelpers;

namespace ReactiveMarbles.RoslynHelpers
{
    /// <summary>
    /// Extension methods for generating formatted syntax.
    /// </summary>
    public static class SyntaxExtensions
    {
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
}
