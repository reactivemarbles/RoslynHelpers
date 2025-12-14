// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using ReactiveMarbles.RoslynHelpers.Helpers;

namespace ReactiveMarbles.RoslynHelpers.Extensions;

/// <summary>
/// Extension methods for <see cref="IncrementalValuesProvider{TValues}"/>.
/// </summary>
public static class IncrementalValuesProviderExtensions
{
    /// <summary>
    /// Groups items in a given <see cref="IncrementalValuesProvider{TValue}"/> sequence by a specified key.
    /// </summary>
    /// <typeparam name="TLeft">The type of left items in each tuple.</typeparam>
    /// <typeparam name="TRight">The type of right items in each tuple.</typeparam>
    /// <typeparam name="TKey">The type of resulting key elements.</typeparam>
    /// <typeparam name="TElement">The type of resulting projected elements.</typeparam>
    /// <param name="source">The input <see cref="IncrementalValuesProvider{TValues}"/> instance.</param>
    /// <param name="keySelector">The key selection <see cref="Func{T, TResult}"/>.</param>
    /// <param name="elementSelector">The element selection <see cref="Func{T, TResult}"/>.</param>
    /// <returns>An <see cref="IncrementalValuesProvider{TValues}"/> with the grouped results.</returns>
    public static IncrementalValuesProvider<(TKey Key, EquatableArray<TElement> Right)> GroupBy<TLeft, TRight, TKey, TElement>(
        this in IncrementalValuesProvider<(TLeft Left, TRight Right)> source,
        Func<(TLeft Left, TRight Right), TKey> keySelector,
        Func<(TLeft Left, TRight Right), TElement> elementSelector)
        where TLeft : IEquatable<TLeft>
        where TRight : IEquatable<TRight>
        where TKey : IEquatable<TKey>
        where TElement : IEquatable<TElement> =>
            source.Collect().SelectMany((item, token) =>
            {
                Dictionary<TKey, ImmutableArray<TElement>.Builder> map = [];

                foreach (var pair in item)
                {
                    var key = keySelector(pair);
                    var element = elementSelector(pair);

                    if (!map.TryGetValue(key, out var builder))
                    {
                        builder = ImmutableArray.CreateBuilder<TElement>();

                        map.Add(key, builder);
                    }

                    builder.Add(element);
                }

                token.ThrowIfCancellationRequested();

                ImmutableArray<(TKey Key, EquatableArray<TElement> Elements)>.Builder result =
                    ImmutableArray.CreateBuilder<(TKey, EquatableArray<TElement>)>();

                foreach (var entry in map)
                {
                    result.Add((entry.Key, entry.Value.ToImmutable()));
                }

                return result;
            });
}
