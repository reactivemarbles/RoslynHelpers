// Copyright (c) 2019-2024 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ReactiveMarbles.RoslynHelpers.Extensions;

/// <summary>
/// Extension methods for the <see cref="SourceProductionContext"/> type.
/// </summary>
public static class SourceProductionContextExtensions
{
    /// <summary>
    /// Adds a new source file to a target <see cref="SourceProductionContext"/> instance.
    /// </summary>
    /// <param name="context">The input <see cref="SourceProductionContext"/> instance to use.</param>
    /// <param name="name">The name of the source file to add.</param>
    /// <param name="compilationUnit">The <see cref="CompilationUnitSyntax"/> instance representing the syntax tree to add.</param>
    public static void AddSource(this in SourceProductionContext context, string name, CompilationUnitSyntax compilationUnit)
    {
        if (compilationUnit == null)
        {
            return;
        }

#if !ROSLYN_4_3_1_OR_GREATER
        if (name?.Contains("+") == true || name?.Contains("`") == true)
        {
            // We're fine with the extra allocation in the few cases where adjusting the filename is necessary.
            // This will only ever be done when code generation is executed again anyway, which is a slow path.
            name = name.Replace('+', '.').Replace('`', '_');
        }
#endif

        // Add the UTF8 text for the input compilation unit
        context.AddSource(name, compilationUnit.GetText(Encoding.UTF8));
    }
}
