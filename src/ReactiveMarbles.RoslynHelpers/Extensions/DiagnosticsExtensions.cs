// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using Microsoft.CodeAnalysis;
using ReactiveMarbles.RoslynHelpers.Helpers;
using ReactiveMarbles.RoslynHelpers.Models;

namespace ReactiveMarbles.RoslynHelpers.Extensions;

/// <summary>
/// Extension methods for <see cref="GeneratorExecutionContext"/>, specifically for reporting diagnostics.
/// </summary>
public static class DiagnosticsExtensions
{
    /// <summary>
    /// Adds a new diagnostics to the target builder.
    /// </summary>
    /// <param name="diagnostics">The collection of produced <see cref="DiagnosticInfo"/> instances.</param>
    /// <param name="descriptor">The input <see cref="DiagnosticDescriptor"/> for the diagnostics to create.</param>
    /// <param name="symbol">The source <see cref="ISymbol"/> to attach the diagnostics to.</param>
    /// <param name="args">The optional arguments for the formatted message to include.</param>
    public static void Add(
        this ImmutableArrayBuilder<DiagnosticInfo> diagnostics,
        DiagnosticDescriptor descriptor,
        ISymbol symbol,
        params object[] args) => diagnostics.Add(DiagnosticInfo.Create(descriptor, symbol, args));

    /// <summary>
    /// Adds a new diagnostics to the target builder.
    /// </summary>
    /// <param name="diagnostics">The collection of produced <see cref="DiagnosticInfo"/> instances.</param>
    /// <param name="descriptor">The input <see cref="DiagnosticDescriptor"/> for the diagnostics to create.</param>
    /// <param name="node">The source <see cref="SyntaxNode"/> to attach the diagnostics to.</param>
    /// <param name="args">The optional arguments for the formatted message to include.</param>
    public static void Add(
        this ImmutableArrayBuilder<DiagnosticInfo> diagnostics,
        DiagnosticDescriptor descriptor,
        SyntaxNode node,
        params object[] args) => diagnostics.Add(DiagnosticInfo.Create(descriptor, node, args));

    /// <summary>
    /// Registers an output node into an <see cref="IncrementalGeneratorInitializationContext"/> to output diagnostics.
    /// </summary>
    /// <param name="context">The input <see cref="IncrementalGeneratorInitializationContext"/> instance.</param>
    /// <param name="diagnostics">The input <see cref="IncrementalValuesProvider{TValues}"/> sequence of diagnostics.</param>
    public static void ReportDiagnostics(this in IncrementalGeneratorInitializationContext context, in IncrementalValuesProvider<DiagnosticInfo> diagnostics) =>
        context.RegisterSourceOutput(diagnostics, static (context, diagnostic) =>
            context.ReportDiagnostic(diagnostic.ToDiagnostic()));

    /// <summary>
    /// Registers an output node into an <see cref="IncrementalGeneratorInitializationContext"/> to output diagnostics.
    /// </summary>
    /// <param name="context">The input <see cref="IncrementalGeneratorInitializationContext"/> instance.</param>
    /// <param name="diagnostics">The input <see cref="IncrementalValuesProvider{TValues}"/> sequence of diagnostics.</param>
    public static void ReportDiagnostics(this in IncrementalGeneratorInitializationContext context, in IncrementalValuesProvider<EquatableArray<DiagnosticInfo>> diagnostics) =>
        context.RegisterSourceOutput(diagnostics, static (context, diagnostics) =>
        {
            foreach (var diagnostic in diagnostics)
            {
                context.ReportDiagnostic(diagnostic.ToDiagnostic());
            }
        });
}
