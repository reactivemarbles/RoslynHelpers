// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using ReactiveMarbles.RoslynHelpers.Helpers;

namespace ReactiveMarbles.RoslynHelpers.Models;

/// <summary>
/// A model representing a value and an associated set of diagnostic errors.
/// </summary>
/// <typeparam name="TValue">The type of the wrapped value.</typeparam>
/// <param name="Value">The wrapped value for the current result.</param>
/// <param name="Errors">The associated diagnostic errors, if any.</param>
public sealed record Result<TValue>(TValue Value, EquatableArray<DiagnosticInfo> Errors)
    where TValue : IEquatable<TValue>?;
