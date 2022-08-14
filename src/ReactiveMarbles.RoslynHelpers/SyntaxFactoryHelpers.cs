// Copyright (c) 2019-2021 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ReactiveMarbles.RoslynHelpers
{
    /// <summary>
    ///     Versions of the syntax factory that handles some trivia for us automatically, to avoid the NormalizeWhitespace
    ///     command.
    /// </summary>
    public static class SyntaxFactoryHelpers
    {
        private const int LeadingSpacesPerLevel = 4;

        /// <summary>
        /// Gets a carriage return symbol.
        /// </summary>
        public static SyntaxTrivia CarriageReturnLineFeed => SyntaxFactory.CarriageReturnLineFeed;

        /// <summary>
        /// Gets a space symbol.
        /// </summary>
        public static SyntaxTrivia Space => SyntaxFactory.Space;

        /// <summary>Creates a new <see cref="AccessorDeclarationSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <returns>The <see cref="AccessorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AccessorDeclarationSyntax AccessorDeclaration(SyntaxKind kind) =>
            SyntaxFactory.AccessorDeclaration(kind).WithSemicolonToken(Token(SyntaxKind.SemicolonToken));

        /// <summary>Creates a new <see cref="AccessorDeclarationSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <returns>The <see cref="AccessorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AccessorDeclarationSyntax AccessorDeclaration(SyntaxKind kind, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers)
        {
            var modifiersList = TokenList(modifiers);
            var attributesList = List(attributes);
            return SyntaxFactory.AccessorDeclaration(kind, attributesList, modifiersList, default, default).WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="AccessorDeclarationSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="AccessorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AccessorDeclarationSyntax AccessorDeclaration(SyntaxKind kind, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, BlockSyntax body)
        {
            var modifiersList = TokenList(modifiers);
            var attributesList = List(attributes);
            return SyntaxFactory.AccessorDeclaration(kind, attributesList, modifiersList, body, default).WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="AccessorDeclarationSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="AccessorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AccessorDeclarationSyntax AccessorDeclaration(SyntaxKind kind, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, ArrowExpressionClauseSyntax body)
        {
            var modifiersList = TokenList(modifiers);
            var attributesList = List(attributes);
            return SyntaxFactory.AccessorDeclaration(kind, attributesList, modifiersList, default, body).WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="AccessorListSyntax" /> instance.</summary>
        /// <param name="accessors">The accessors.</param>
        /// <returns>The <see cref="AccessorListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AccessorListSyntax AccessorList(IReadOnlyList<AccessorDeclarationSyntax>? accessors)
        {
            if (accessors is null || accessors.Count == 0)
            {
                return SyntaxFactory.AccessorList();
            }

            return SyntaxFactory.AccessorList(Token(SyntaxKind.OpenBraceToken).AddTrialingSpaces().AddLeadingSpaces(), List(GetSpacedAccessors(accessors)), Token(SyntaxKind.CloseBraceToken).AddLeadingSpaces());
        }

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <param name="argumentName">The argument name.</param>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax Argument(string argumentName) => SyntaxFactory.Argument(IdentifierName(argumentName));

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <param name="argumentName">The argument name.</param>
        /// <param name="refKind">The reference kind.</param>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax Argument(string argumentName, in SyntaxToken refKind) => SyntaxFactory.Argument(null, refKind, IdentifierName(argumentName));

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <param name="argumentName">The argument name.</param>
        /// <param name="nameColon">A name column this argument points towards.</param>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax Argument(string argumentName, string nameColon) => SyntaxFactory.Argument(NameColon(nameColon), default, IdentifierName(argumentName));

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <param name="expression">The expression of the argument.</param>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax Argument(ExpressionSyntax expression) => SyntaxFactory.Argument(expression);

        /// <summary>Creates a new <see cref="ArgumentListSyntax" /> instance.</summary>
        /// <returns>The <see cref="ArgumentListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentListSyntax ArgumentList() => SyntaxFactory.ArgumentList();

        /// <summary>Creates a new <see cref="ArgumentListSyntax" /> instance.</summary>
        /// <param name="nodes">The arguments.</param>
        /// <returns>The <see cref="ArgumentListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentListSyntax ArgumentList(IReadOnlyCollection<ArgumentSyntax> nodes)
        {
            if (nodes is null || nodes.Count == 0)
            {
                return ArgumentList();
            }

            return SyntaxFactory.ArgumentList(nodes.Count == 1 ? SingletonSeparatedList(nodes.First()) : SeparatedList(nodes));
        }

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax ArgumentThis() => SyntaxFactory.Argument(ThisExpression());

        /// <summary>Creates a new <see cref="ArrayCreationExpressionSyntax" /> instance.</summary>
        /// <param name="type">The type of array.</param>
        /// <returns>The <see cref="ArrayCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayCreationExpressionSyntax ArrayCreationExpression(ArrayTypeSyntax type) => SyntaxFactory.ArrayCreationExpression(SyntaxFactory.Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), type, default);

        /// <summary>Creates a new <see cref="ArrayCreationExpressionSyntax" /> instance.</summary>
        /// <param name="type">the type of array.</param>
        /// <param name="initializer">The array initializer.</param>
        /// <returns>The <see cref="ArrayCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayCreationExpressionSyntax ArrayCreationExpression(ArrayTypeSyntax type, InitializerExpressionSyntax initializer) => SyntaxFactory.ArrayCreationExpression(Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), type, initializer);

        /// <summary>Creates a new <see cref="ArrayRankSpecifierSyntax" /> instance.</summary>
        /// <param name="rank">The rank of the array.</param>
        /// <returns>The <see cref="ArrayRankSpecifierSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayRankSpecifierSyntax ArrayRankSpecifier(OmittedArraySizeExpressionSyntax rank) => SyntaxFactory.ArrayRankSpecifier(SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(rank));

        /// <summary>Creates a new <see cref="ArrayRankSpecifierSyntax" /> instance.</summary>
        /// <returns>The <see cref="ArrayRankSpecifierSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayRankSpecifierSyntax ArrayRankSpecifier() => SyntaxFactory.ArrayRankSpecifier();

        /// <summary>Creates a new <see cref="ArrayRankSpecifierSyntax" /> instance.</summary>
        /// <param name="sizes">The sizes of the array.</param>
        /// <returns>The <see cref="ArrayRankSpecifierSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayRankSpecifierSyntax ArrayRankSpecifier(IReadOnlyCollection<int?> sizes)
        {
            var sizeSpecifier = sizes.Select(x => x is null ? (ExpressionSyntax)OmittedArraySizeExpression() : LiteralExpression(x.Value)).ToList();
            return SyntaxFactory.ArrayRankSpecifier(SeparatedList(sizeSpecifier));
        }

        /// <summary>Creates a new <see cref="ArrayTypeSyntax" /> instance.</summary>
        /// <param name="elementType">The type of element.</param>
        /// <param name="rankSpecifiers">The rank specifiers.</param>
        /// <returns>The <see cref="ArrayTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrayTypeSyntax ArrayType(TypeSyntax elementType, IReadOnlyCollection<ArrayRankSpecifierSyntax> rankSpecifiers)
        {
            var rank = rankSpecifiers is null || rankSpecifiers.Count == 0 ? List(new[] { ArrayRankSpecifier() }) : List(rankSpecifiers);
            return SyntaxFactory.ArrayType(elementType, rank);
        }

        /// <summary>Creates a new <see cref="ArrowExpressionClauseSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ArrowExpressionClauseSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArrowExpressionClauseSyntax ArrowExpressionClause(ExpressionSyntax expression) => SyntaxFactory.ArrowExpressionClause(expression);

        /// <summary>Creates a new <see cref="AssignmentExpressionSyntax" /> instance.</summary>
        /// <param name="token">The token.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="AssignmentExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind token, ExpressionSyntax left, ExpressionSyntax right) => SyntaxFactory.AssignmentExpression(token, left, Token(GetAssignmentExpressionOperatorTokenKind(token)).AddLeadingSpaces().AddTrialingSpaces(), right);

        /// <summary>Creates a new <see cref="AssignmentExpressionSyntax" /> instance.</summary>
        /// <param name="token">The token.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="AssignmentExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind token, ExpressionSyntax left, string right) => AssignmentExpression(token, left, IdentifierName(right));

        /// <summary>Creates a new <see cref="AssignmentExpressionSyntax" /> instance.</summary>
        /// <param name="token">The token.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="AssignmentExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind token, string left, ExpressionSyntax right) => AssignmentExpression(token, IdentifierName(left), right);

        /// <summary>Creates a new <see cref="AssignmentExpressionSyntax" /> instance.</summary>
        /// <param name="token">The token.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="AssignmentExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind token, string left, string right) => AssignmentExpression(token, IdentifierName(left), IdentifierName(right));

        /// <summary>Creates a new <see cref="AttributeSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="AttributeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeSyntax Attribute(string name) => Attribute(name, default);

        /// <summary>Creates a new <see cref="AttributeSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="AttributeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeSyntax Attribute(string name, IReadOnlyCollection<AttributeArgumentSyntax>? arguments)
        {
            if (name is null)
            {
                throw new ArgumentNullException(nameof(name));
            }

            var argumentsList = AttributeArgumentList(arguments);

            if (name.EndsWith("Attribute", StringComparison.InvariantCulture))
            {
                var lastIndex = name.LastIndexOf("Attribute", StringComparison.InvariantCulture);
                name = name.Substring(0, lastIndex);
            }

            return SyntaxFactory.Attribute(IdentifierName(name), argumentsList);
        }

        /// <summary>Creates a new <see cref="AttributeArgumentSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="AttributeArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeArgumentSyntax AttributeArgument(ExpressionSyntax expression) => SyntaxFactory.AttributeArgument(expression);

        /// <summary>Creates a new <see cref="AttributeArgumentSyntax" /> instance.</summary>
        /// <param name="nameEquals">The name equals value.</param>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="AttributeArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeArgumentSyntax AttributeArgument(NameEqualsSyntax nameEquals, ExpressionSyntax expression) => SyntaxFactory.AttributeArgument(nameEquals, default, expression);

        /// <summary>Creates a new <see cref="AttributeArgumentListSyntax" /> instance.</summary>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="AttributeArgumentListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeArgumentListSyntax? AttributeArgumentList(IReadOnlyCollection<AttributeArgumentSyntax>? arguments)
        {
            if (arguments is null || arguments.Count == 0)
            {
                return default;
            }

            return SyntaxFactory.AttributeArgumentList(arguments.Count == 1 ? SingletonSeparatedList(arguments.First()) : SeparatedList(arguments));
        }

        /// <summary>Creates a new <see cref="AttributeListSyntax" /> instance.</summary>
        /// <param name="attribute">The attribute.</param>
        /// <returns>The <see cref="AttributeListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeListSyntax AttributeList(AttributeSyntax attribute) => AttributeList(attribute, default);

        /// <summary>Creates a new <see cref="AttributeListSyntax" /> instance.</summary>
        /// <param name="attribute">The attribute.</param>
        /// <param name="target">The target type.</param>
        /// <returns>The <see cref="AttributeListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeListSyntax AttributeList(AttributeSyntax attribute, SyntaxKind? target)
        {
            var attributeList = SingletonSeparatedList(attribute);

            AttributeTargetSpecifierSyntax? attributeTarget = null;
            if (target is not null)
            {
                attributeTarget = AttributeTargetSpecifier(target.Value);
            }

            return SyntaxFactory.AttributeList(Token(SyntaxKind.OpenBracketToken), attributeTarget, attributeList, Token(SyntaxKind.CloseBracketToken));
        }

        /// <summary>Creates a new <see cref="AttributeTargetSpecifierSyntax" /> instance.</summary>
        /// <param name="target">The target type.</param>
        /// <returns>The <see cref="AttributeTargetSpecifierSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static AttributeTargetSpecifierSyntax AttributeTargetSpecifier(SyntaxKind target) => SyntaxFactory.AttributeTargetSpecifier(Token(target), Token(SyntaxKind.ColonToken).AddTrialingSpaces());

        /// <summary>Creates a new <see cref="BaseListSyntax" /> instance.</summary>
        /// <param name="baseType">The base type.</param>
        /// <returns>The <see cref="BaseListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BaseListSyntax? BaseList(BaseTypeSyntax baseType) =>
            baseType is null ? default : SyntaxFactory.BaseList(Token(SyntaxKind.ColonToken).AddLeadingSpaces().AddTrialingSpaces(), SingletonSeparatedList(baseType));

        /// <summary>Creates a new <see cref="BaseListSyntax" /> instance.</summary>
        /// <param name="baseItems">The base types.</param>
        /// <returns>The <see cref="BaseListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BaseListSyntax? BaseList(IReadOnlyCollection<BaseTypeSyntax>? baseItems)
        {
            if (baseItems is null || baseItems.Count == 0)
            {
                return default;
            }

            return SyntaxFactory.BaseList(Token(SyntaxKind.ColonToken).AddLeadingSpaces().AddTrialingSpaces(), SeparatedList(baseItems));
        }

        /// <summary>Creates a new <see cref="BinaryExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="BinaryExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BinaryExpressionSyntax BinaryExpression(SyntaxKind kind, ExpressionSyntax left, ExpressionSyntax right) => SyntaxFactory.BinaryExpression(kind, left.AddTrialingSpaces(), right.AddLeadingSpaces());

        /// <summary>Creates a new <see cref="BinaryExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>The <see cref="BinaryExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BinaryExpressionSyntax BinaryExpression(SyntaxKind kind, string left, ExpressionSyntax right) => BinaryExpression(kind, IdentifierName(left), right);

        /// <summary>Creates a new <see cref="BlockSyntax" /> instance.</summary>
        /// <param name="statements">The statements to add.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="BlockSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BlockSyntax Block(IReadOnlyCollection<StatementSyntax> statements, int level)
        {
            var statementsList = List(statements, level + 1);
            var (openingBrace, closingBrace) = GetBraces(level);
            return SyntaxFactory.Block(default, openingBrace, statementsList, closingBrace);
        }

        /// <summary>Creates a new <see cref="BracketedArgumentListSyntax" /> instance.</summary>
        /// <param name="nodes">The nodes.</param>
        /// <returns>The <see cref="BracketedArgumentListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BracketedArgumentListSyntax BracketedArgumentList(IReadOnlyCollection<ArgumentSyntax> nodes)
        {
            if (nodes is null || nodes.Count == 0)
            {
                return SyntaxFactory.BracketedArgumentList();
            }

            var (openBrace, closeBrace) = GetBrackets();

            return SyntaxFactory.BracketedArgumentList(
                openBrace,
                nodes.Count == 1 ? SingletonSeparatedList(nodes.First()) : SeparatedList(nodes),
                closeBrace);
        }

        /// <summary>
        /// Creates a new <see cref="CastExpressionSyntax" /> instance.
        /// </summary>
        /// <param name="type">The type to cast.</param>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="CastExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static CastExpressionSyntax CastExpression(string type, ExpressionSyntax expression)
        {
            var openParen = Token(SyntaxKind.OpenParenToken);
            var closeParen = Token(SyntaxKind.CloseParenToken);
            return SyntaxFactory.CastExpression(openParen, IdentifierName(type), closeParen, expression);
        }

        /// <summary>
        /// Creates a new <see cref="CastExpressionSyntax" /> instance.
        /// </summary>
        /// <param name="type">The type to cast.</param>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="CastExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static CastExpressionSyntax CastExpression(TypeSyntax type, ExpressionSyntax expression)
        {
            var openParen = Token(SyntaxKind.OpenParenToken);
            var closeParen = Token(SyntaxKind.CloseParenToken);
            return SyntaxFactory.CastExpression(openParen, type, closeParen, expression);
        }

        /// <summary>Creates a new <see cref="ClassDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ClassDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassDeclarationSyntax ClassDeclaration(string identifier, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, int level) =>
            ClassDeclaration(identifier, default, modifiers, members, default, default, default, level);

        /// <summary>Creates a new <see cref="ClassDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="bases">The base types.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ClassDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassDeclarationSyntax ClassDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, IReadOnlyCollection<BaseTypeSyntax> bases, int level) =>
            ClassDeclaration(identifier, attributes, modifiers, members, default, default, bases, level);

        /// <summary>Creates a new <see cref="ClassDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ClassDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassDeclarationSyntax ClassDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, int level) =>
            ClassDeclaration(identifier, attributes, modifiers, members, default, default, default, level);

        /// <summary>Creates a new <see cref="ClassDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ClassDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassDeclarationSyntax ClassDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, int level) =>
            ClassDeclaration(identifier, attributes, modifiers, members, default, typeParameters, default, level);

        /// <summary>Creates a new <see cref="ClassDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="typeParameterConstraintClauses">The type parameters constraint clauses.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="bases">The bases.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ClassDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassDeclarationSyntax ClassDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, IReadOnlyCollection<TypeParameterConstraintClauseSyntax>? typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, IReadOnlyCollection<BaseTypeSyntax>? bases, int level)
        {
            var classSyntax = Token(SyntaxKind.ClassKeyword);
            GetTypeValues(identifier, attributes, modifiers, members, typeParameterConstraintClauses, typeParameters, bases, level, out var attributesList, out var name, out var modifiersList, out var baseList, out var membersList, out var typeParameterList, out var typeParameterConstraintList, out var openingBrace, out var closingBrace);

            return SyntaxFactory.ClassDeclaration(attributesList, modifiersList, classSyntax, name, typeParameterList, baseList, typeParameterConstraintList, openingBrace, membersList, closingBrace, default);
        }

        /// <summary>Creates a new <see cref="ClassOrStructConstraintSyntax" /> instance.</summary>
        /// <param name="syntaxKind">The syntax kind.</param>
        /// <returns>The <see cref="ClassOrStructConstraintSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ClassOrStructConstraintSyntax ClassOrStructConstraint(SyntaxKind syntaxKind) => SyntaxFactory.ClassOrStructConstraint(syntaxKind);

        /// <summary>Creates a new <see cref="CompilationUnitSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="members">The members.</param>
        /// <param name="usings">Using statements of the compilation statement.</param>
        /// <returns>The <see cref="CompilationUnitSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static CompilationUnitSyntax CompilationUnit(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<MemberDeclarationSyntax> members, IReadOnlyCollection<UsingDirectiveSyntax>? usings)
        {
            var attributesList = List(attributes, 0, true);
            var membersList = List(members);
            var usingList = List(usings);
            return SyntaxFactory.CompilationUnit(default, usingList, attributesList, membersList);
        }

        /// <summary>Creates a new <see cref="ConditionalAccessExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <param name="whenNotNull">The when not null expression.</param>
        /// <returns>The <see cref="ConditionalAccessExpressionSyntax" /> instance.</returns>
        public static ConditionalAccessExpressionSyntax ConditionalAccessExpression(ExpressionSyntax expression, ExpressionSyntax whenNotNull) => SyntaxFactory.ConditionalAccessExpression(expression, Token(SyntaxKind.QuestionToken).AddTrialingSpaces(), whenNotNull);

        /// <summary>Creates a new <see cref="ConstructorConstraintSyntax" /> instance.</summary>
        /// <returns>The <see cref="ConstructorConstraintSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ConstructorConstraintSyntax ConstructorConstraint() => SyntaxFactory.ConstructorConstraint();

        /// <summary>Creates a new <see cref="ConstructorDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="ConstructorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ConstructorDeclarationSyntax ConstructorDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyCollection<ParameterSyntax> parameters, string identifier, int level)
        {
            var name = Identifier(identifier);
            var parametersList = ParameterList(parameters);
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            return SyntaxFactory.ConstructorDeclaration(attributeList, modifiersList, name, parametersList, default, default, default, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="ConstructorDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="block">The block.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="ConstructorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ConstructorDeclarationSyntax ConstructorDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyCollection<ParameterSyntax> parameters, string identifier, BlockSyntax block, int level)
        {
            var name = Identifier(identifier);
            var parametersList = ParameterList(parameters);
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            return SyntaxFactory.ConstructorDeclaration(attributeList, modifiersList, name, parametersList, default, block, default, default);
        }

        /// <summary>Creates a new <see cref="ConversionOperatorDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="implicitOrExplicitKeyword">If its implicit or explicit.</param>
        /// <param name="type">The type.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="ConversionOperatorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ConversionOperatorDeclarationSyntax ConversionOperatorDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, in SyntaxToken implicitOrExplicitKeyword, string type, IReadOnlyCollection<ParameterSyntax> parameters, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var typeName = IdentifierName(type).AddLeadingSpaces();

            var parametersList = ParameterList(parameters);

            return SyntaxFactory.ConversionOperatorDeclaration(attributeList, modifiersList, implicitOrExplicitKeyword, Token(SyntaxKind.OperatorKeyword).AddTrialingSpaces().AddLeadingSpaces(), typeName, parametersList, default, default, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="DelegateDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="returnType">The return type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="typeParameterConstraintClauses">The type constraint clauses.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="DelegateDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static DelegateDeclarationSyntax DelegateDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, TypeSyntax returnType, string identifier, IReadOnlyCollection<ParameterSyntax> parameters, IReadOnlyCollection<TypeParameterConstraintClauseSyntax> typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax> typeParameters, int level)
        {
            var attributeList = List(attributes, level, true);
            var name = Identifier(identifier).AddLeadingSpaces();
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var typeParameterList = TypeParameterList(typeParameters);
            var typeParameterConstraintList = List(typeParameterConstraintClauses);
            var parametersList = ParameterList(parameters);
            return SyntaxFactory.DelegateDeclaration(attributeList, modifiersList, Token(SyntaxKind.DelegateKeyword), returnType.AddLeadingSpaces(), name, typeParameterList, parametersList, typeParameterConstraintList, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="DestructorDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="DestructorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static DestructorDeclarationSyntax DestructorDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, string identifier, int level)
        {
            var name = Identifier(identifier).AddLeadingSpaces();
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            return SyntaxFactory.DestructorDeclaration(attributeList, modifiersList, Token(SyntaxKind.TildeToken), name, ParameterList(), default, default, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="ElementAccessExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ElementAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ElementAccessExpressionSyntax ElementAccessExpression(ExpressionSyntax expression) => SyntaxFactory.ElementAccessExpression(expression);

        /// <summary>Creates a new <see cref="ElementAccessExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ElementAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ElementAccessExpressionSyntax ElementAccessExpression(string expression) => SyntaxFactory.ElementAccessExpression(IdentifierName(expression));

        /// <summary>Creates a new <see cref="ElementAccessExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="ElementAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ElementAccessExpressionSyntax ElementAccessExpression(string expression, IReadOnlyCollection<ArgumentSyntax> arguments)
        {
            var argumentsList = SeparatedList(arguments);
            var bracketedArgumentList = BracketedArgumentList(argumentsList);

            return SyntaxFactory.ElementAccessExpression(IdentifierName(expression), bracketedArgumentList);
        }

        /// <summary>Creates a new <see cref="EnumDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="members">the members.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="baseIdentifier">The identifier of the base type.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="EnumDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static EnumDeclarationSyntax EnumDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<EnumMemberDeclarationSyntax> members, IReadOnlyCollection<SyntaxKind> modifiers, string baseIdentifier, int level)
        {
            var attributeList = List(attributes, level, true);
            var name = Identifier(identifier).AddLeadingSpaces();
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var membersList = SeparatedList(members, level + 1);

            var baseList = !string.IsNullOrWhiteSpace(baseIdentifier) ? BaseList(SimpleBaseType(IdentifierName(baseIdentifier))) : default;

            var (openingBrace, closingBrace) = GetBraces(level);
            return SyntaxFactory.EnumDeclaration(attributeList, modifiersList, Token(SyntaxKind.EnumKeyword), name, baseList, openingBrace, membersList, closingBrace, default);
        }

        /// <summary>Creates a new <see cref="EnumMemberDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="equalsValue">The values equal.</param>
        /// <returns>The <see cref="EnumMemberDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static EnumMemberDeclarationSyntax EnumMemberDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, string identifier, EqualsValueClauseSyntax equalsValue)
        {
            var attributesList = List(attributes);
            var name = SyntaxFactory.Identifier(identifier);
            return SyntaxFactory.EnumMemberDeclaration(attributesList, name, equalsValue);
        }

        /// <summary>Creates a new <see cref="EqualsValueClauseSyntax" /> instance.</summary>
        /// <param name="value">The expression value.</param>
        /// <returns>The <see cref="EqualsValueClauseSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static EqualsValueClauseSyntax EqualsValueClause(ExpressionSyntax value) => SyntaxFactory.EqualsValueClause(SyntaxFactory.Token(SyntaxKind.EqualsToken).AddLeadingSpaces().AddTrialingSpaces(), value);

        /// <summary>Creates a new <see cref="EventFieldDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="eventType">The event types.</param>
        /// <param name="eventName">The name of the event.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="EventFieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static EventFieldDeclarationSyntax EventFieldDeclaration(IReadOnlyCollection<SyntaxKind> modifiers, string eventType, string eventName, int level)
        {
            var variableDeclaration = VariableDeclaration(IdentifierName(eventType), SeparatedList(new[] { VariableDeclarator(eventName) }));

            return EventFieldDeclaration(default, modifiers, variableDeclaration, level);
        }

        /// <summary>Creates a new <see cref="EventFieldDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="declaration">The declaration.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="EventFieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static EventFieldDeclarationSyntax EventFieldDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, VariableDeclarationSyntax declaration, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            return SyntaxFactory.EventFieldDeclaration(attributeList, modifiersList, SyntaxFactory.Token(SyntaxKind.EventKeyword).AddTrialingSpaces(), declaration, SyntaxFactory.Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="ExplicitInterfaceSpecifierSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ExplicitInterfaceSpecifierSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ExplicitInterfaceSpecifierSyntax ExplicitInterfaceSpecifier(string name) => SyntaxFactory.ExplicitInterfaceSpecifier(IdentifierName(name));

        /// <summary>Creates a new <see cref="ExpressionStatementSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ExpressionStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression) =>
            SyntaxFactory.ExpressionStatement(default, expression, Token(SyntaxKind.SemicolonToken));

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(string type, string name, IReadOnlyCollection<SyntaxKind>? modifiers, int level) => FieldDeclaration(type, name, default, modifiers, level);

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(TypeSyntax type, string name, IReadOnlyCollection<SyntaxKind>? modifiers, int level) => FieldDeclaration(type, name, Array.Empty<AttributeListSyntax>(), modifiers, level);

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(string type, string name, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, int level) =>
            FieldDeclaration(IdentifierName(type), name, attributes, modifiers, level);

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(TypeSyntax type, string name, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = modifiers?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            var variableDeclaration = VariableDeclaration(type, SeparatedList(new[] { VariableDeclarator(name) }));

            return SyntaxFactory.FieldDeclaration(attributeList, modifiersList, variableDeclaration, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="initializer">The initializer.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(TypeSyntax type, string name, EqualsValueClauseSyntax initializer, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind> modifiers, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = modifiers?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            var variableDeclaration = VariableDeclaration(type, SeparatedList(new[] { VariableDeclarator(name, initializer) }));

            return SyntaxFactory.FieldDeclaration(attributeList, modifiersList, variableDeclaration, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="initializer">The initializer.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(string type, string name, EqualsValueClauseSyntax initializer, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind> modifiers, int level) =>
            FieldDeclaration(IdentifierName(type), name, initializer, attributes, modifiers, level);

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="initializer">The initializer.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(TypeSyntax type, string name, EqualsValueClauseSyntax initializer, IReadOnlyCollection<SyntaxKind> modifiers, int level)
        {
            var modifiersList = modifiers?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            var variableDeclaration = VariableDeclaration(type, SeparatedList(new[] { VariableDeclarator(name, initializer) }));

            return SyntaxFactory.FieldDeclaration(default, modifiersList, variableDeclaration, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="FieldDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="declaration">the declaration.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="FieldDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static FieldDeclarationSyntax FieldDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, VariableDeclarationSyntax declaration, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            return SyntaxFactory.FieldDeclaration(attributeList, modifiersList, declaration, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="ForEachStatementSyntax" /> instance.</summary>
        /// <param name="typeName">The type name.</param>
        /// <param name="name">the name.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="statement">The statement.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ForEachStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ForEachStatementSyntax ForEachStatement(string typeName, string name, ExpressionSyntax expression, StatementSyntax statement, int level) =>
            ForEachStatement(Array.Empty<AttributeListSyntax>(), typeName, name, expression, statement, level);

        /// <summary>Creates a new <see cref="ForEachStatementSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="typeName">The type name.</param>
        /// <param name="name">the name.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="statement">The statement.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="ForEachStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ForEachStatementSyntax ForEachStatement(IReadOnlyCollection<AttributeListSyntax> attributes, string typeName, string name, ExpressionSyntax expression, StatementSyntax statement, int level)
        {
            var attributeList = List(attributes, level, true);
            var type = IdentifierName(typeName).AddTrialingSpaces();
            var identifier = Identifier(name).AddTrialingSpaces();
            return SyntaxFactory.ForEachStatement(attributeList, default, Token(SyntaxKind.ForEachKeyword).AddTrialingSpaces(), Token(SyntaxKind.OpenParenToken), type, identifier, Token(SyntaxKind.InKeyword).AddTrialingSpaces(), expression, Token(SyntaxKind.CloseParenToken), statement);
        }

        /// <summary>Creates a new <see cref="ForStatementSyntax" /> instance.</summary>
        /// <param name="declaration">The declaration.</param>
        /// <param name="initializers">The initializers.</param>
        /// <param name="conditional">the conditional.</param>
        /// <param name="incrementers">The incrementers.</param>
        /// <param name="statement">The statement.</param>
        /// <returns>The <see cref="ForStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ForStatementSyntax ForStatement(VariableDeclarationSyntax declaration, IReadOnlyList<ExpressionSyntax> initializers, ExpressionSyntax conditional, IReadOnlyList<ExpressionSyntax> incrementers, StatementSyntax statement)
        {
            var initializerList = SeparatedList(initializers);
            var incrementersList = SeparatedList(incrementers);
            return SyntaxFactory.ForStatement(default, Token(SyntaxKind.ForKeyword).AddTrialingSpaces(), Token(SyntaxKind.OpenParenToken), declaration, initializerList, Token(SyntaxKind.SemicolonToken).AddTrialingSpaces(), conditional, Token(SyntaxKind.SemicolonToken).AddTrialingSpaces(), incrementersList, Token(SyntaxKind.CloseParenToken), statement);
        }

        /// <summary>Creates a new <see cref="GenericNameSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="GenericNameSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static GenericNameSyntax GenericName(string name) => SyntaxFactory.GenericName(name);

        /// <summary>Creates a new <see cref="GenericNameSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <param name="types">The types.</param>
        /// <returns>The <see cref="GenericNameSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static GenericNameSyntax GenericName(string name, IReadOnlyCollection<TypeSyntax> types)
        {
            var typesList = TypeArgumentList(types);
            return SyntaxFactory.GenericName(Identifier(name), typesList);
        }

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="text">The text.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Identifier(string text) => SyntaxFactory.Identifier(text);

        /// <summary>Creates a new <see cref="IdentifierNameSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="IdentifierNameSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static IdentifierNameSyntax IdentifierName(string name) => SyntaxFactory.IdentifierName(name);

        /// <summary>Creates a new <see cref="IfStatementSyntax" /> instance.</summary>
        /// <param name="conditionStatement">The conditional statement.</param>
        /// <param name="statement">If statement to run if true.</param>
        /// <returns>The <see cref="IfStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static IfStatementSyntax IfStatement(ExpressionSyntax conditionStatement, StatementSyntax statement) =>
            SyntaxFactory.IfStatement(default, Token(SyntaxKind.IfKeyword).AddTrialingSpaces(), Token(SyntaxKind.OpenParenToken), conditionStatement, Token(SyntaxKind.CloseParenToken), statement, null);

        /// <summary>Creates a new <see cref="IfStatementSyntax" /> instance.</summary>
        /// <param name="conditionStatement">The conditional statement.</param>
        /// <param name="statement">If statement to run if true.</param>
        /// <param name="elseStatement">The else statement to run if else.</param>
        /// <returns>The <see cref="IfStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static IfStatementSyntax IfStatement(ExpressionSyntax conditionStatement, StatementSyntax statement, ElseClauseSyntax elseStatement) =>
            SyntaxFactory.IfStatement(default, Token(SyntaxKind.IfKeyword).AddTrialingSpaces(), Token(SyntaxKind.OpenParenToken), conditionStatement, Token(SyntaxKind.CloseParenToken), statement, elseStatement);

        /// <summary>Creates a new <see cref="ImplicitElementAccessSyntax" /> instance.</summary>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="ImplicitElementAccessSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ImplicitElementAccessSyntax ImplicitElementAccess(IReadOnlyCollection<ArgumentSyntax> arguments) =>
            SyntaxFactory.ImplicitElementAccess(BracketedArgumentList(arguments));

        /// <summary>Creates a new <see cref="InitializerExpressionSyntax" /> instance.</summary>
        /// <param name="syntaxKind">The type of syntax.</param>
        /// <param name="expressions">The expressions.</param>
        /// <returns>The <see cref="InitializerExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InitializerExpressionSyntax InitializerExpression(SyntaxKind syntaxKind, IReadOnlyCollection<ExpressionSyntax> expressions)
        {
            var (openBrace, closeBrace) = GetBraces();

            var list = SeparatedList(expressions);

            return SyntaxFactory.InitializerExpression(syntaxKind, openBrace, list, closeBrace);
        }

        /// <summary>Creates a new <see cref="InterfaceDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">the type of members.</param>
        /// <param name="typeParameterConstraintClauses">The type constraints.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="bases">The type of bases.</param>
        /// <param name="level">The level of indentation.</param>
        /// <returns>The <see cref="InterfaceDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InterfaceDeclarationSyntax InterfaceDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyCollection<MemberDeclarationSyntax> members, IReadOnlyCollection<TypeParameterConstraintClauseSyntax> typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax> typeParameters, IReadOnlyCollection<BaseTypeSyntax> bases, int level)
        {
            GetTypeValues(identifier, attributes, modifiers, members, typeParameterConstraintClauses, typeParameters, bases, level, out var attributesList, out var name, out var modifiersList, out var baseList, out var membersList, out var typeParameterList, out var typeParameterConstraintList, out var openingBrace, out var closingBrace);

            var typeIdentifier = Token(SyntaxKind.InterfaceKeyword);

            return SyntaxFactory.InterfaceDeclaration(attributesList, modifiersList, typeIdentifier, name, typeParameterList, baseList, typeParameterConstraintList, openingBrace, membersList, closingBrace, default);
        }

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <param name="methodName">The name of the method.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvocationExpression(string methodName, IReadOnlyCollection<ArgumentSyntax> arguments) =>
            InvocationExpression(IdentifierName(methodName), arguments);

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <param name="methodName">The method name.</param>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvocationExpression(string methodName) =>
            InvocationExpression(IdentifierName(methodName));

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvocationExpression(ExpressionSyntax expression) =>
            InvocationExpression(expression, Array.Empty<ArgumentSyntax>());

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvocationExpression(ExpressionSyntax expression, IReadOnlyCollection<ArgumentSyntax> arguments) =>
            SyntaxFactory.InvocationExpression(expression, ArgumentList(arguments));

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <param name="className">The class name.</param>
        /// <param name="methodName">The method name.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvokeExplicitMethod(string className, string methodName, params ArgumentSyntax[] arguments) =>
            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName(className), IdentifierName(methodName)), arguments);

        /// <summary>Creates a new <see cref="InvocationExpressionSyntax" /> instance.</summary>
        /// <returns>The <see cref="InvocationExpressionSyntax" /> instance.</returns>
        /// <param name="methodName">The method name.</param>
        /// <param name="instance">The instance.</param>
        /// <param name="arguments">The arguments.</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static InvocationExpressionSyntax InvokeMethod(string methodName, ExpressionSyntax instance, params ArgumentSyntax[] arguments) =>
            InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, instance, IdentifierName(methodName)), arguments);

        /// <summary>Creates a new <see cref="ArgumentSyntax" /> instance.</summary>
        /// <param name="variableName">The variable name.</param>
        /// <param name="members">the members to access.</param>
        /// <returns>The <see cref="ArgumentSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ArgumentSyntax LambdaAccessArgument(string variableName, MemberAccessExpressionSyntax members) =>
            Argument(SimpleLambdaExpression(Parameter(variableName), members));

        /// <summary>
        /// Creates a new <see cref="SyntaxList{TNode}" /> instance.
        /// </summary>
        /// <typeparam name="TNode">The type of node.</typeparam>
        /// <returns>The <see cref="SyntaxList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxList<TNode> List<TNode>()
            where TNode : SyntaxNode => default;

        /// <summary>
        /// Creates a new <see cref="SyntaxList{TNode}" /> instance.
        /// </summary>
        /// <typeparam name="TNode">The type of node.</typeparam>
        /// <param name="nodes">The nodes.</param>
        /// <returns>The <see cref="SyntaxList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxList<TNode> List<TNode>(IReadOnlyCollection<TNode>? nodes)
            where TNode : SyntaxNode
        {
            if (nodes is null || nodes.Count == 0)
            {
                return default;
            }

            return SyntaxFactory.List(nodes);
        }

        /// <summary>
        /// Creates a new <see cref="SyntaxList{TNode}" /> instance.
        /// </summary>
        /// <typeparam name="TNode">The type of node.</typeparam>
        /// <param name="nodes">The nodes.</param>
        /// <param name="level">The level to indent the values.</param>
        /// <param name="lastNodeTrailingLine">If the last node should have a trailing line.</param>
        /// <returns>The <see cref="SyntaxList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxList<TNode> List<TNode>(IReadOnlyCollection<TNode>? nodes, int level, bool lastNodeTrailingLine = false)
            where TNode : SyntaxNode
        {
            if (nodes is null || nodes.Count == 0)
            {
                return default;
            }

            return SyntaxFactory.List(GetIndentedNodes(nodes, level, lastNodeTrailingLine));
        }

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(ulong value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(uint value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(int value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(long value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(char value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(double value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(float value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Literal(string value) => SyntaxFactory.Literal(value);

        /// <summary>Creates a new <see cref="LiteralExpressionSyntax" /> instance.</summary>
        /// <param name="syntaxKind">The syntax kind.</param>
        /// <returns>The <see cref="LiteralExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LiteralExpressionSyntax LiteralExpression(SyntaxKind syntaxKind) => SyntaxFactory.LiteralExpression(syntaxKind);

        /// <summary>Creates a new <see cref="LiteralExpressionSyntax" /> instance.</summary>
        /// <param name="syntaxKind">The syntax kind.</param>
        /// <param name="token">The token.</param>
        /// <returns>The <see cref="LiteralExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LiteralExpressionSyntax LiteralExpression(SyntaxKind syntaxKind, in SyntaxToken token) => SyntaxFactory.LiteralExpression(syntaxKind, token);

        /// <summary>Creates a new <see cref="LiteralExpressionSyntax" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="LiteralExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LiteralExpressionSyntax LiteralExpression(string value) => SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(value));

        /// <summary>Creates a new <see cref="LiteralExpressionSyntax" /> instance.</summary>
        /// <param name="value">The value.</param>
        /// <returns>The <see cref="LiteralExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LiteralExpressionSyntax LiteralExpression(int value) => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(value));

        /// <summary>Creates a new <see cref="LocalDeclarationStatementSyntax" /> instance.</summary>
        /// <param name="declaration">The declaration.</param>
        /// <returns>The <see cref="LocalDeclarationStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LocalDeclarationStatementSyntax LocalDeclarationStatement(VariableDeclarationSyntax declaration) => SyntaxFactory.LocalDeclarationStatement(default, default, default, default, declaration, Token(SyntaxKind.SemicolonToken));

        /// <summary>Creates a new <see cref="LocalFunctionStatementSyntax" /> instance.</summary>
        /// <param name="returnType">The return type.</param>
        /// <param name="functionName">The function name.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="expressionBody">The body.</param>
        /// <returns>The <see cref="LocalFunctionStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LocalFunctionStatementSyntax LocalFunctionStatement(string returnType, string functionName, IReadOnlyCollection<ParameterSyntax>? parameters, ArrowExpressionClauseSyntax expressionBody)
        {
            var parameterList = ParameterList(parameters);

            return SyntaxFactory.LocalFunctionStatement(default, default, IdentifierName(returnType).AddTrialingSpaces(), Identifier(functionName), default, parameterList, default, default, expressionBody, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="startVariable">The starting variable.</param>
        /// <param name="identifierNames">The identifiers.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccess(string startVariable, params string[] identifierNames) =>
            (MemberAccessExpressionSyntax)identifierNames.Aggregate<string, ExpressionSyntax>(IdentifierName(startVariable), (expression, name) =>
                MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, IdentifierName(name)));

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="startVariable">The starting variable.</param>
        /// <param name="identifierNames">The identifiers.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccess(ExpressionSyntax startVariable, params string[] identifierNames) =>
            (MemberAccessExpressionSyntax)identifierNames.Aggregate(startVariable, (expression, name) =>
                MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, IdentifierName(name)));

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="startVariable">The starting variable.</param>
        /// <param name="identifierNames">The identifiers.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccess(ExpressionSyntax startVariable, IEnumerable<string> identifierNames) =>
            MemberAccess(startVariable, identifierNames.ToArray());

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="startVariable">The starting variable.</param>
        /// <param name="identifierNames">The identifiers.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccess(string startVariable, IEnumerable<string> identifierNames) =>
            MemberAccess(startVariable, identifierNames.ToArray());

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax expression, SimpleNameSyntax name) => SyntaxFactory.MemberAccessExpression(kind, expression, name);

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="name1">The first name.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, string name1, SimpleNameSyntax name) => SyntaxFactory.MemberAccessExpression(kind, IdentifierName(name1), name);

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax expression, string name) => SyntaxFactory.MemberAccessExpression(kind, expression, IdentifierName(name));

        /// <summary>Creates a new <see cref="MemberAccessExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="MemberAccessExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, string expression, string name) => SyntaxFactory.MemberAccessExpression(kind, IdentifierName(expression), IdentifierName(name));

        /// <summary>Creates a new <see cref="MemberBindingExpressionSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="MemberBindingExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MemberBindingExpressionSyntax MemberBindingExpression(string name) => SyntaxFactory.MemberBindingExpression(Token(SyntaxKind.DotToken).AddTrialingSpaces(), IdentifierName(name));

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, BlockSyntax body) =>
            MethodDeclaration(default, modifiers, type, default, identifier, parameters, default, default, body, default, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, BlockSyntax body) =>
            MethodDeclaration(attributes, modifiers, IdentifierName(typeName), default, identifier, parameters, default, default, body, default, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(attributes, modifiers, IdentifierName(typeName), default, identifier, parameters, default, default, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, BlockSyntax body) =>
            MethodDeclaration(attributes, modifiers, type, default, identifier, parameters, default, default, body, default, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, string identifier, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, type, default, identifier, default, default, default, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, type, default, identifier, parameters, default, default, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, BlockSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, parameters, default, default, body, default, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, parameters, default, default, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, default, default, default, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, int level, BlockSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, parameters, default, typeParameters, body, default, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, parameters, default, typeParameters, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, string typeName, string identifier, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, IdentifierName(typeName), default, identifier, default, default, typeParameters, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="level">The level.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, string identifier, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, int level, ArrowExpressionClauseSyntax body) =>
            MethodDeclaration(default, modifiers, type, default, identifier, default, default, typeParameters, default, body, level);

        /// <summary>Creates a new <see cref="MethodDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="explicitInterface">The explicit interface.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="typeParameterConstraintClauses">The type constraint clauses.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="body">The body.</param>
        /// <param name="arrowSyntax">The arrow syntax.</param>
        /// <param name="level">The level.</param>
        /// <returns>The <see cref="MethodDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static MethodDeclarationSyntax MethodDeclaration(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax type, ExplicitInterfaceSpecifierSyntax? explicitInterface, string identifier, IReadOnlyCollection<ParameterSyntax>? parameters, IReadOnlyCollection<TypeParameterConstraintClauseSyntax>? typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, BlockSyntax? body, ArrowExpressionClauseSyntax? arrowSyntax, int level)
        {
            var name = SyntaxFactory.Identifier(identifier);
            if (explicitInterface is null)
            {
                name = name.AddLeadingSpaces();
            }

            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            var typeParameterList = TypeParameterList(typeParameters);
            var typeParameterConstraintList = typeParameterConstraintClauses?.Count > 0 ? List(typeParameterConstraintClauses, level + 1) : default;

            var parametersList = ParameterList(parameters);

            explicitInterface = explicitInterface?.AddLeadingSpaces();

            var token = body == default ? Token(SyntaxKind.SemicolonToken) : default;

            return SyntaxFactory.MethodDeclaration(attributeList, modifiersList, type, explicitInterface, name, typeParameterList, parametersList, typeParameterConstraintList, body, arrowSyntax, token);
        }

        /// <summary>Creates a new <see cref="NameColonSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="NameColonSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static NameColonSyntax NameColon(string name) => SyntaxFactory.NameColon(IdentifierName(name), Token(SyntaxKind.ColonToken).AddTrialingSpaces());

        /// <summary>Creates a new <see cref="NameEqualsSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="NameEqualsSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static NameEqualsSyntax NameEquals(IdentifierNameSyntax name) => SyntaxFactory.NameEquals(name);

        /// <summary>Creates a new <see cref="NamespaceDeclarationSyntax" /> instance.</summary>
        /// <param name="nameText">The name.</param>
        /// <param name="members">The members.</param>
        /// <param name="createLeadingNewLine">If a leading new line should be generated.</param>
        /// <returns>The <see cref="NamespaceDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static NamespaceDeclarationSyntax NamespaceDeclaration(string nameText, IReadOnlyCollection<MemberDeclarationSyntax> members, bool createLeadingNewLine)
        {
            var name = IdentifierName(nameText).AddLeadingSpaces();

            var membersList = List(members, 1);

            var namespaceToken = createLeadingNewLine ? Token(SyntaxKind.NamespaceKeyword).AddLeadingNewLines() : Token(SyntaxKind.NamespaceKeyword);

            return SyntaxFactory.NamespaceDeclaration(namespaceToken, name, Token(SyntaxKind.OpenBraceToken).AddLeadingNewLines(), default, default, membersList, Token(SyntaxKind.CloseBraceToken).AddLeadingNewLines(), default);
        }

        /// <summary>Creates a new <see cref="NullableTypeSyntax" /> instance.</summary>
        /// <param name="typeSyntax">The type.</param>
        /// <returns>The <see cref="NullableTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static NullableTypeSyntax NullableType(TypeSyntax typeSyntax) => SyntaxFactory.NullableType(typeSyntax);

        /// <summary>Creates a new <see cref="LiteralExpressionSyntax" /> instance.</summary>
        /// <returns>The <see cref="LiteralExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static LiteralExpressionSyntax NullLiteral() => SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression);

        /// <summary>Creates a new <see cref="ObjectCreationExpressionSyntax" /> instance.</summary>
        /// <param name="typeName">The type name.</param>
        /// <param name="arguments">The arguments.</param>
        /// <param name="initializer">The initializer.</param>
        /// <returns>The <see cref="ObjectCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ObjectCreationExpressionSyntax ObjectCreationExpression(string typeName, IReadOnlyCollection<ArgumentSyntax> arguments, InitializerExpressionSyntax initializer)
        {
            var argumentList = ArgumentList(arguments);
            return SyntaxFactory.ObjectCreationExpression(Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), IdentifierName(typeName), argumentList, initializer);
        }

        /// <summary>Creates a new <see cref="ObjectCreationExpressionSyntax" /> instance.</summary>
        /// <param name="typeName">The type name.</param>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="ObjectCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ObjectCreationExpressionSyntax ObjectCreationExpression(string typeName, IReadOnlyCollection<ArgumentSyntax> arguments)
        {
            var argumentList = ArgumentList(arguments);
            return SyntaxFactory.ObjectCreationExpression(Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), IdentifierName(typeName), argumentList, default);
        }

        /// <summary>Creates a new <see cref="ObjectCreationExpressionSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="arguments">The arguments.</param>
        /// <param name="initializer">The initializer.</param>
        /// <returns>The <see cref="ObjectCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ObjectCreationExpressionSyntax ObjectCreationExpression(TypeSyntax type, IReadOnlyCollection<ArgumentSyntax> arguments, InitializerExpressionSyntax initializer)
        {
            var argumentList = ArgumentList(arguments);
            return SyntaxFactory.ObjectCreationExpression(Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), type, argumentList, initializer);
        }

        /// <summary>Creates a new <see cref="ObjectCreationExpressionSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="ObjectCreationExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ObjectCreationExpressionSyntax ObjectCreationExpression(TypeSyntax type) => SyntaxFactory.ObjectCreationExpression(Token(SyntaxKind.NewKeyword).AddTrialingSpaces(), type, default, default);

        /// <summary>Creates a new <see cref="OmittedArraySizeExpressionSyntax" /> instance.</summary>
        /// <returns>The <see cref="OmittedArraySizeExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static OmittedArraySizeExpressionSyntax OmittedArraySize() => SyntaxFactory.OmittedArraySizeExpression();

        /// <summary>Creates a new <see cref="OmittedArraySizeExpressionSyntax" /> instance.</summary>
        /// <returns>The <see cref="OmittedArraySizeExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static OmittedArraySizeExpressionSyntax OmittedArraySizeExpression() => SyntaxFactory.OmittedArraySizeExpression();

        /// <summary>Creates a new <see cref="OperatorDeclarationSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="parameters">The parameters.</param>
        /// <param name="returnType">The return type.</param>
        /// <param name="operatorToken">The operator token.</param>
        /// <param name="level">The identation level.</param>
        /// <returns>The <see cref="OperatorDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static OperatorDeclarationSyntax OperatorDeclaration(IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyCollection<ParameterSyntax> parameters, TypeSyntax returnType, in SyntaxToken operatorToken, int level)
        {
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var parametersList = ParameterList(parameters);
            return SyntaxFactory.OperatorDeclaration(attributeList, modifiersList, returnType, Token(SyntaxKind.OperatorKeyword).AddTrialingSpaces().AddLeadingSpaces(), operatorToken, parametersList, default, default, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(string name) => Parameter(default, default, default, name, default);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <param name="modifier">The modifier.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(string name, IReadOnlyCollection<SyntaxKind> modifier) => Parameter(default, modifier, default, name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="modifier">The type of modifier.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(string type, string name, IReadOnlyCollection<SyntaxKind> modifier) => Parameter(default, modifier, IdentifierName(type).AddTrialingSpaces(), name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(string type, string name) => Parameter(default, default, IdentifierName(type).AddTrialingSpaces(), name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="equals">The equals clause.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(string type, string name, EqualsValueClauseSyntax equals) => Parameter(default, default, IdentifierName(type).AddTrialingSpaces(), name, equals);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(IReadOnlyCollection<AttributeListSyntax>? attributes, string type, string name) => Parameter(attributes, default, IdentifierName(type).AddTrialingSpaces(), name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(IReadOnlyCollection<AttributeListSyntax>? attributes, TypeSyntax type, string name) => Parameter(attributes, default, type.AddTrialingSpaces(), name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="equals">The equals clause.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(IReadOnlyCollection<AttributeListSyntax>? attributes, string type, string name, EqualsValueClauseSyntax equals) => Parameter(attributes, default, IdentifierName(type).AddTrialingSpaces(), name, equals);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <param name="equals">The equals clause.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(IReadOnlyCollection<AttributeListSyntax>? attributes, TypeSyntax type, string name, EqualsValueClauseSyntax equals) => Parameter(attributes, default, type.AddTrialingSpaces(), name, equals);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="name">The name.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(TypeSyntax type, string name) => Parameter(default, default, type.AddTrialingSpaces(), name, null);

        /// <summary>Creates a new <see cref="ParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The name.</param>
        /// <param name="equals">The equals clause.</param>
        /// <returns>The <see cref="ParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterSyntax Parameter(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, TypeSyntax? type, string identifier, EqualsValueClauseSyntax? equals)
        {
            var name = SyntaxFactory.Identifier(identifier);
            type = type is null ? default : type.AddTrialingSpaces();
            var modifiersList = TokenList(modifiers);
            var attributesList = List(attributes);
            equals = equals?.AddLeadingSpaces();

            return SyntaxFactory.Parameter(attributesList, modifiersList, type, name, equals);
        }

        /// <summary>Creates a new <see cref="ParameterListSyntax" /> instance.</summary>
        /// <param name="parameters">The parameters.</param>
        /// <returns>The <see cref="ParameterListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterListSyntax ParameterList(IReadOnlyCollection<ParameterSyntax>? parameters)
        {
            if (parameters is null || parameters.Count == 0)
            {
                return SyntaxFactory.ParameterList();
            }

            var (openParentheses, closeParentheses) = GetParentheses();

            return SyntaxFactory.ParameterList(openParentheses, SeparatedList(parameters), closeParentheses);
        }

        /// <summary>Creates a new <see cref="ParameterListSyntax" /> instance.</summary>
        /// <returns>The <see cref="ParameterListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParameterListSyntax ParameterList() => SyntaxFactory.ParameterList();

        /// <summary>Creates a new <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="block">The block.</param>
        /// <returns>The <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(BlockSyntax block) => ParenthesizedLambdaExpression(default, block);

        /// <summary>Creates a new <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="parameters">The parameters.</param>
        /// <param name="block">The block.</param>
        /// <returns>The <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(IReadOnlyCollection<ParameterSyntax>? parameters, BlockSyntax block)
        {
            var parameterList = parameters is null || parameters.Count == 0 ? ParameterList() : ParameterList(parameters);

            return SyntaxFactory.ParenthesizedLambdaExpression(default, parameterList, Token(SyntaxKind.EqualsGreaterThanToken).AddTrialingSpaces().AddLeadingSpaces(), block);
        }

        /// <summary>Creates a new <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="block">The block.</param>
        /// <returns>The <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(ExpressionSyntax block) => ParenthesizedLambdaExpression(default, block);

        /// <summary>Creates a new <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="parameters">The parameters.</param>
        /// <param name="block">The block.</param>
        /// <returns>The <see cref="ParenthesizedLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(IReadOnlyCollection<ParameterSyntax>? parameters, ExpressionSyntax block)
        {
            var parameterList = parameters is null || parameters.Count == 0 ? ParameterList() : ParameterList(parameters);

            return SyntaxFactory.ParenthesizedLambdaExpression(default, parameterList, Token(SyntaxKind.EqualsGreaterThanToken).AddTrialingSpaces().AddLeadingSpaces(), block);
        }

        /// <summary>Creates a new <see cref="PointerTypeSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="PointerTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PointerTypeSyntax PointerType(TypeSyntax type) => SyntaxFactory.PointerType(type);

        /// <summary>Creates a new <see cref="PrefixUnaryExpressionSyntax" /> instance.</summary>
        /// <param name="kind">The syntax kind.</param>
        /// <param name="operand">The operand.</param>
        /// <returns>The <see cref="PrefixUnaryExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PrefixUnaryExpressionSyntax PrefixUnaryExpression(SyntaxKind kind, ExpressionSyntax operand) =>
            SyntaxFactory.PrefixUnaryExpression(kind, operand);

        /// <summary>Creates a new <see cref="PropertyDeclarationSyntax" /> instance.</summary>
        /// <param name="typeName">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="accessors">The accessors.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="PropertyDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PropertyDeclarationSyntax PropertyDeclaration(string typeName, string identifier, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyList<AccessorDeclarationSyntax> accessors, int level) =>
            PropertyDeclaration(IdentifierName(typeName), identifier, modifiers, accessors, level);

        /// <summary>Creates a new <see cref="PropertyDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="accessors">The accessors.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="PropertyDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PropertyDeclarationSyntax PropertyDeclaration(TypeSyntax type, string identifier, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyList<AccessorDeclarationSyntax> accessors, int level)
        {
            var name = Identifier(identifier).AddLeadingSpaces();
            var modifiersList = modifiers?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var accessorList = AccessorList(accessors);

            return SyntaxFactory.PropertyDeclaration(default, modifiersList, type, default, name, accessorList, default, default, default);
        }

        /// <summary>Creates a new <see cref="PropertyDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="accessors">The accessors.</param>
        /// <param name="initializer">The initializer.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="PropertyDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PropertyDeclarationSyntax PropertyDeclaration(TypeSyntax type, string identifier, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyList<AccessorDeclarationSyntax> accessors, EqualsValueClauseSyntax initializer, int level) =>
            PropertyDeclaration(type, identifier, default, modifiers, accessors, initializer, level);

        /// <summary>Creates a new <see cref="PropertyDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="expressionBody">The expression body.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="PropertyDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PropertyDeclarationSyntax PropertyDeclaration(TypeSyntax type, string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, ArrowExpressionClauseSyntax expressionBody, int level)
        {
            var name = Identifier(identifier).AddLeadingSpaces();
            var attributeList = List(attributes, level, true);
            var modifiersList = modifiers?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

            return SyntaxFactory.PropertyDeclaration(attributeList, modifiersList, type, default, name, default, expressionBody, default);
        }

        /// <summary>Creates a new <see cref="PropertyDeclarationSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="accessors">The accessors.</param>
        /// <param name="initializer">The initializers.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="PropertyDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PropertyDeclarationSyntax PropertyDeclaration(TypeSyntax type, string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyList<AccessorDeclarationSyntax>? accessors, EqualsValueClauseSyntax? initializer, int level)
        {
            var name = Identifier(identifier).AddLeadingSpaces();
            var attributeList = List(attributes, level, true);
            var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            var accessorList = AccessorList(accessors);

            return SyntaxFactory.PropertyDeclaration(attributeList, modifiersList, type, default, name, accessorList, default, initializer, Token(SyntaxKind.SemicolonToken));
        }

        /// <summary>Creates a new <see cref="QualifiedNameSyntax" /> instance.</summary>
        /// <param name="left">The left name.</param>
        /// <param name="right">The right name.</param>
        /// <returns>The <see cref="QualifiedNameSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static QualifiedNameSyntax QualifiedName(NameSyntax left, SimpleNameSyntax right) => SyntaxFactory.QualifiedName(left, right);

        /// <summary>Creates a new <see cref="QualifiedNameSyntax" /> instance.</summary>
        /// <param name="left">The left name.</param>
        /// <param name="right">The right name.</param>
        /// <returns>The <see cref="QualifiedNameSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static QualifiedNameSyntax QualifiedName(string left, string right) => SyntaxFactory.QualifiedName(IdentifierName(left), IdentifierName(right));

        /// <summary>Creates a new <see cref="RefTypeSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <param name="isReadOnly">If the value is read only.</param>
        /// <returns>The <see cref="RefTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static RefTypeSyntax RefType(TypeSyntax type, bool isReadOnly)
        {
            var readOnlySyntax = isReadOnly ? Token(SyntaxKind.ReadOnlyKeyword).AddTrialingSpaces() : Token(SyntaxKind.None);
            return SyntaxFactory.RefType(Token(SyntaxKind.RefKeyword).AddTrialingSpaces(), readOnlySyntax, type);
        }

        /// <summary>Creates a new <see cref="ReturnStatementSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ReturnStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ReturnStatementSyntax ReturnStatement(ExpressionSyntax expression) => SyntaxFactory.ReturnStatement(default, Token(SyntaxKind.ReturnKeyword).AddTrialingSpaces(), expression, Token(SyntaxKind.SemicolonToken));

        /// <summary>Creates a new <see cref="ReturnStatementSyntax" /> instance.</summary>
        /// <param name="value">The value to return.</param>
        /// <returns>The <see cref="ReturnStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ReturnStatementSyntax ReturnStatement(string value) => SyntaxFactory.ReturnStatement(default, Token(SyntaxKind.ReturnKeyword).AddTrialingSpaces(), IdentifierName(value), Token(SyntaxKind.SemicolonToken));

        /// <summary>
        /// Creates a new <see cref="SeparatedSyntaxList{TNode}"/> instance.
        /// </summary>
        /// <typeparam name="TNode">The type of node.</typeparam>
        /// <param name="nodes">The nodes for the list.</param>
        /// <returns>The <see cref="SeparatedSyntaxList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SeparatedSyntaxList<TNode> SeparatedList<TNode>(IReadOnlyCollection<TNode> nodes)
            where TNode : SyntaxNode
        {
            if (nodes is null || nodes.Count == 0)
            {
                return default;
            }

            if (nodes.Count == 1)
            {
                return SingletonSeparatedList(nodes.First());
            }

            var commaSeparation = Enumerable.Repeat(Token(SyntaxKind.CommaToken).AddTrialingSpaces(), nodes.Count - 1);
            return SyntaxFactory.SeparatedList(nodes, commaSeparation);
        }

        /// <summary>
        /// Creates a new <see cref="SeparatedList{TNode}"/> instance.
        /// </summary>
        /// <param name="nodes">The nodes.</param>
        /// <param name="level">the indentation level.</param>
        /// <returns>The <see cref="SeparatedList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SeparatedSyntaxList<EnumMemberDeclarationSyntax> SeparatedList(IReadOnlyCollection<EnumMemberDeclarationSyntax> nodes, int level)
        {
            if (nodes is null || nodes.Count == 0)
            {
                return default;
            }

            return nodes.Count == 1 ? SingletonSeparatedList(nodes.First()) : SyntaxFactory.SeparatedList(GetIndentedNodes(nodes, level));
        }

        /// <summary>Creates a new <see cref="SimpleBaseTypeSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="SimpleBaseTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleBaseTypeSyntax SimpleBaseType(TypeSyntax type) => SyntaxFactory.SimpleBaseType(type);

        /// <summary>Creates a new <see cref="SimpleBaseTypeSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="SimpleBaseTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleBaseTypeSyntax SimpleBaseType(string type) => SyntaxFactory.SimpleBaseType(IdentifierName(type));

        /// <summary>Creates a new <see cref="SimpleLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="parameter">The parameter.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="SimpleLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(ParameterSyntax parameter, ExpressionSyntax body) =>
            SimpleLambdaExpression(parameter, default, body);

        /// <summary>Creates a new <see cref="SimpleLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="parameter">The parameter.</param>
        /// <param name="body">The body.</param>
        /// <returns>The <see cref="SimpleLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(ParameterSyntax parameter, BlockSyntax body) =>
            SimpleLambdaExpression(parameter, body, default);

        /// <summary>Creates a new <see cref="SimpleLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="parameter">The parameter.</param>
        /// <param name="block">The optional block. Use this or expression body.</param>
        /// <param name="expressionBody">The optional expression body. Use this or block.</param>
        /// <returns>The <see cref="SimpleLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(ParameterSyntax parameter, BlockSyntax? block, ExpressionSyntax? expressionBody)
        {
            var token = Token(SyntaxKind.EqualsGreaterThanToken).AddLeadingSpaces().AddTrialingSpaces();

            return SyntaxFactory.SimpleLambdaExpression(default(SyntaxTokenList), parameter, token, block, expressionBody);
        }

        /// <summary>Creates a new <see cref="SimpleLambdaExpressionSyntax" /> instance.</summary>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="parameter">The parameter.</param>
        /// <param name="block">The optional block. Use this or expression body.</param>
        /// <param name="expressionBody">The optional expression body. Use this or block.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="SimpleLambdaExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(IReadOnlyCollection<SyntaxKind>? modifiers, ParameterSyntax parameter, BlockSyntax? block, ExpressionSyntax? expressionBody, int level)
        {
            var modifiersList = TokenList(modifiers);

            var token = Token(SyntaxKind.EqualsGreaterThanToken).AddLeadingSpaces().AddTrialingSpaces();

            return SyntaxFactory.SimpleLambdaExpression(modifiersList, parameter, token, block, expressionBody);
        }

        /////// <summary>Creates a new <see cref="SimpleLambdaExpressionSyntax" /> instance.</summary>
        /////// <param name="attributes">The attributes.</param>
        /////// <param name="modifiers">The modifiers.</param>
        /////// <param name="parameter">The parameter.</param>
        /////// <param name="block">The optional block. Use this or expression body.</param>
        /////// <param name="expressionBody">The optional expression body. Use this or block.</param>
        /////// <param name="level">The indentation level.</param>
        /////// <returns>The <see cref="SimpleLambdaExpressionSyntax" /> instance.</returns>
        ////[MethodImpl(MethodImplOptions.AggressiveInlining)]
        ////public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, ParameterSyntax parameter, BlockSyntax? block, ExpressionSyntax? expressionBody, int level)
        ////{
        ////    var attributeList = List(attributes, level, true);
        ////    var modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);

        ////    var token = Token(SyntaxKind.EqualsGreaterThanToken).AddLeadingSpaces().AddTrialingSpaces();

        ////    return SyntaxFactory.SimpleLambdaExpression(attributeList, modifiersList, parameter, token, block, expressionBody);
        ////}

        /// <summary>
        /// Creates a new <see cref="SeparatedSyntaxList{TNode}"/> instance.
        /// </summary>
        /// <typeparam name="TNode">The type of node.</typeparam>
        /// <param name="node">The node.</param>
        /// <returns>The <see cref="SeparatedSyntaxList{TNode}"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SeparatedSyntaxList<TNode> SingletonSeparatedList<TNode>(TNode node)
            where TNode : SyntaxNode => SyntaxFactory.SingletonSeparatedList(node);

        /// <summary>Creates a new <see cref="StructDeclarationSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <param name="attributes">The attributes.</param>
        /// <param name="modifiers">The modifiers.</param>
        /// <param name="members">The members.</param>
        /// <param name="typeParameterConstraintClauses">The type parameter constraint clauses.</param>
        /// <param name="typeParameters">The type parameters.</param>
        /// <param name="bases">The bases.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="StructDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static StructDeclarationSyntax StructDeclaration(string identifier, IReadOnlyCollection<AttributeListSyntax> attributes, IReadOnlyCollection<SyntaxKind> modifiers, IReadOnlyCollection<MemberDeclarationSyntax> members, IReadOnlyCollection<TypeParameterConstraintClauseSyntax> typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax> typeParameters, IReadOnlyCollection<BaseTypeSyntax> bases, int level)
        {
            GetTypeValues(identifier, attributes, modifiers, members, typeParameterConstraintClauses, typeParameters, bases, level, out var attributesList, out var name, out var modifiersList, out var baseList, out var membersList, out var typeParameterList, out var typeParameterConstraintList, out var openingBrace, out var closingBrace);
            var typeIdentifier = Token(SyntaxKind.StructKeyword);

            return SyntaxFactory.StructDeclaration(attributesList, modifiersList, typeIdentifier, name, typeParameterList, baseList, typeParameterConstraintList, openingBrace, membersList, closingBrace, default);
        }

        /// <summary>Creates a new <see cref="ThisExpressionSyntax" /> instance.</summary>
        /// <returns>The <see cref="ThisExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ThisExpressionSyntax ThisExpression() => SyntaxFactory.ThisExpression();

        /// <summary>Creates a new <see cref="ThrowStatementSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="expression">The expression.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="ThrowStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ThrowStatementSyntax ThrowStatement(IReadOnlyCollection<AttributeListSyntax> attributes, ExpressionSyntax expression, int level)
        {
            var attributeList = List(attributes, level, true);

            return SyntaxFactory.ThrowStatement(attributeList, Token(SyntaxKind.ThrowKeyword).AddTrialingSpaces(), expression, Token(SyntaxKind.SemicolonToken).AddTrialingNewLines());
        }

        /// <summary>Creates a new <see cref="ThrowStatementSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <param name="level">The indentation level.</param>
        /// <returns>The <see cref="ThrowStatementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ThrowStatementSyntax ThrowStatement(ExpressionSyntax expression, int level)
        {
            return SyntaxFactory.ThrowStatement(default, Token(SyntaxKind.ThrowKeyword).AddTrialingSpaces(), expression, Token(SyntaxKind.SemicolonToken).AddTrialingNewLines());
        }

        /// <summary>Creates a new <see cref="ThrowExpressionSyntax" /> instance.</summary>
        /// <param name="expression">The expression.</param>
        /// <returns>The <see cref="ThrowExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static ThrowExpressionSyntax ThrowExpression(ExpressionSyntax expression)
        {
            return SyntaxFactory.ThrowExpression(Token(SyntaxKind.ThrowKeyword).AddTrialingSpaces(), expression);
        }

        /// <summary>Creates a new <see cref="SyntaxToken" /> instance.</summary>
        /// <param name="kind">The kind of syntax.</param>
        /// <returns>The <see cref="SyntaxToken" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxToken Token(SyntaxKind kind) => SyntaxFactory.Token(kind);

        /// <summary>Creates a new <see cref="SyntaxTokenList" /> instance.</summary>
        /// <param name="tokens">The tokens.</param>
        /// <returns>The <see cref="SyntaxTokenList" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxTokenList TokenList(IReadOnlyCollection<SyntaxKind>? tokens)
        {
            if (tokens is null || tokens.Count == 0)
            {
                return default;
            }

            var items = new List<SyntaxToken>(tokens.Count);

            foreach (var token in tokens)
            {
                items.Add(Token(token).AddTrialingSpaces());
            }

            return SyntaxFactory.TokenList(items);
        }

        /// <summary>Creates a new <see cref="SyntaxTokenList" /> instance.</summary>
        /// <param name="tokens">The tokens.</param>
        /// <param name="level">The indenting level.</param>
        /// <returns>The <see cref="SyntaxTokenList" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static SyntaxTokenList TokenList(IReadOnlyCollection<SyntaxKind>? tokens, int level)
        {
            if (tokens is null || tokens.Count == 0)
            {
                return default;
            }

            var items = new List<SyntaxToken>(tokens.Count);

            var i = 0;

            foreach (var token in tokens)
            {
                items.Add(i == 0 ? Token(token).AddLeadingSpaces(level * LeadingSpacesPerLevel).AddTrialingSpaces() : Token(token).AddTrialingSpaces());

                i++;
            }

            return SyntaxFactory.TokenList(items);
        }

        /// <summary>Creates a new <see cref="TupleElementSyntax" /> instance.</summary>
        /// <param name="type">The type of tuple element.</param>
        /// <param name="name">The name of the tuple element.</param>
        /// <returns>The <see cref="TupleElementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TupleElementSyntax TupleElement(TypeSyntax type, string name)
        {
            var identifier = string.IsNullOrWhiteSpace(name) ? default : Identifier(name);
            type = identifier == default ? type : type.AddTrialingSpaces();
            return SyntaxFactory.TupleElement(type, identifier);
        }

        /// <summary>Creates a new <see cref="TupleElementSyntax" /> instance.</summary>
        /// <param name="type">The type of tuple element.</param>
        /// <param name="name">The name of the tuple element.</param>
        /// <returns>The <see cref="TupleElementSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TupleElementSyntax TupleElement(string type, string name) => TupleElement(IdentifierName(type), name);

        /// <summary>Creates a new <see cref="TupleExpressionSyntax" /> instance.</summary>
        /// <param name="arguments">The arguments.</param>
        /// <returns>The <see cref="TupleExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TupleExpressionSyntax TupleExpression(IReadOnlyCollection<ArgumentSyntax> arguments)
        {
            var argumentLists = SeparatedList(arguments);
            var (openBracket, closeBracket) = GetParentheses();
            return SyntaxFactory.TupleExpression(openBracket, argumentLists, closeBracket);
        }

        /// <summary>Creates a new <see cref="TupleTypeSyntax" /> instance.</summary>
        /// <param name="elements">The tuple elements.</param>
        /// <returns>The <see cref="TupleTypeSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TupleTypeSyntax TupleType(IReadOnlyCollection<TupleElementSyntax> elements) => SyntaxFactory.TupleType(SeparatedList(elements));

        /// <summary>Creates a new <see cref="TypeArgumentListSyntax" /> instance.</summary>
        /// <param name="types">The types.</param>
        /// <returns>The <see cref="TypeArgumentListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeArgumentListSyntax TypeArgumentList(IReadOnlyCollection<TypeSyntax> types) => types is null || types.Count == 0 ? SyntaxFactory.TypeArgumentList() : SyntaxFactory.TypeArgumentList(SeparatedList(types));

        /// <summary>Creates a new <see cref="TypeConstraintSyntax" /> instance.</summary>
        /// <param name="typeName">The type name.</param>
        /// <returns>The <see cref="TypeConstraintSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeConstraintSyntax TypeConstraint(TypeSyntax typeName) => SyntaxFactory.TypeConstraint(typeName);

        /// <summary>Creates a new <see cref="TypeOfExpressionSyntax" /> instance.</summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="TypeOfExpressionSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeOfExpressionSyntax TypeOfExpression(TypeSyntax type) => SyntaxFactory.TypeOfExpression(Token(SyntaxKind.TypeOfKeyword), Token(SyntaxKind.OpenParenToken), type, Token(SyntaxKind.CloseParenToken));

        /// <summary>Creates a new <see cref="TypeParameterSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <returns>The <see cref="TypeParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeParameterSyntax TypeParameter(string identifier) => TypeParameter(default, default, identifier);

        /// <summary>Creates a new <see cref="TypeParameterSyntax" /> instance.</summary>
        /// <param name="attributes">The attributes.</param>
        /// <param name="varianceKind">The kind of variance.</param>
        /// <param name="identifier">The identifier.</param>
        /// <returns>The <see cref="TypeParameterSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeParameterSyntax TypeParameter(IReadOnlyCollection<AttributeListSyntax>? attributes, SyntaxKind varianceKind, string identifier)
        {
            var name = Identifier(identifier);
            var varianceKeyword = varianceKind == SyntaxKind.None ? default : Token(varianceKind).AddTrialingSpaces();
            var attributesList = List(attributes);
            return SyntaxFactory.TypeParameter(attributesList, varianceKeyword, name);
        }

        /// <summary>Creates a new <see cref="TypeParameterConstraintClauseSyntax" /> instance.</summary>
        /// <param name="name">The name.</param>
        /// <param name="constraints">The constraints.</param>
        /// <returns>The <see cref="TypeParameterConstraintClauseSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeParameterConstraintClauseSyntax TypeParameterConstraintClause(string name, IReadOnlyCollection<TypeParameterConstraintSyntax> constraints)
        {
            var constraintsList = SeparatedList(constraints);
            return SyntaxFactory.TypeParameterConstraintClause(Token(SyntaxKind.WhereKeyword).AddLeadingSpaces().AddTrialingSpaces(), IdentifierName(name), Token(SyntaxKind.ColonToken).AddLeadingSpaces().AddTrialingSpaces(), constraintsList);
        }

        /// <summary>Creates a new <see cref="TypeParameterListSyntax" /> instance.</summary>
        /// <param name="parameters">The parameters.</param>
        /// <returns>The <see cref="TypeParameterListSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static TypeParameterListSyntax? TypeParameterList(IReadOnlyCollection<TypeParameterSyntax>? parameters)
        {
            if (parameters is null || parameters.Count == 0)
            {
                return default;
            }

            return SyntaxFactory.TypeParameterList(SeparatedList(parameters));
        }

        /// <summary>Creates a new <see cref="UsingDirectiveSyntax" /> instance.</summary>
        /// <param name="identifier">The identifier.</param>
        /// <returns>The <see cref="UsingDirectiveSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static UsingDirectiveSyntax UsingDirective(string identifier) => SyntaxFactory.UsingDirective(Token(SyntaxKind.UsingKeyword).AddTrialingSpaces(), default, null, IdentifierName(identifier), Token(SyntaxKind.SemicolonToken).AddTrialingNewLines());

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclarationSyntax VariableDeclaration(TypeSyntax type) => SyntaxFactory.VariableDeclaration(type);

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="variableDeclaratorSyntaxes">The declarators.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclarationSyntax VariableDeclaration(TypeSyntax type, IReadOnlyCollection<VariableDeclaratorSyntax> variableDeclaratorSyntaxes)
        {
            var variableDeclaratorList = SeparatedList(variableDeclaratorSyntaxes);
            return SyntaxFactory.VariableDeclaration(type.AddTrialingSpaces(), variableDeclaratorList);
        }

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="variableDeclaratorSyntaxes">The declarators.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>

        /// <summary>Creates a new <see cref="VariableDeclarationSyntax" /> instance.</summary>
        /// <returns>The <see cref="VariableDeclarationSyntax" /> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclarationSyntax VariableDeclaration(string type, IReadOnlyCollection<VariableDeclaratorSyntax> variableDeclaratorSyntaxes)
        {
            var variableDeclaratorList = SeparatedList(variableDeclaratorSyntaxes);
            return SyntaxFactory.VariableDeclaration(IdentifierName(type).AddTrialingSpaces(), variableDeclaratorList);
        }

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclarationSyntax VariableDeclaration(string type) => SyntaxFactory.VariableDeclaration(IdentifierName(type), default);

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="identifier">The identifier.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclaratorSyntax VariableDeclarator(string identifier)
        {
            var name = SyntaxFactory.Identifier(identifier);
            return SyntaxFactory.VariableDeclarator(name, default, default);
        }

        /// <summary>
        /// Creates a <see cref="VariableDeclarationSyntax"/> instance.
        /// </summary>
        /// <param name="identifier">A identifier.</param>
        /// <param name="initializer">What the variable equals.</param>
        /// <returns>The <see cref="VariableDeclarationSyntax"/> instance.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static VariableDeclaratorSyntax VariableDeclarator(string identifier, EqualsValueClauseSyntax initializer)
        {
            var name = SyntaxFactory.Identifier(identifier);
            return SyntaxFactory.VariableDeclarator(name, default, initializer);
        }

        /// <summary>
        /// Gets the void type.
        /// </summary>
        /// <returns>The void type.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static PredefinedTypeSyntax Void() => SyntaxFactory.PredefinedType(Token(SyntaxKind.VoidKeyword));

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static (SyntaxToken OpeningBrace, SyntaxToken ClosingBrace) GetBraces()
        {
            var openingBrace = Token(SyntaxKind.OpenBraceToken).AddTrialingSpaces();
            var closingBrace = Token(SyntaxKind.CloseBraceToken).AddTrialingSpaces();

            return (openingBrace, closingBrace);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static (SyntaxToken OpeningBrace, SyntaxToken ClosingBrace) GetBraces(int level)
        {
            var openingBrace = Token(SyntaxKind.OpenBraceToken).AddLeadingNewLinesAndSpaces(1, level * LeadingSpacesPerLevel);
            var closingBrace = Token(SyntaxKind.CloseBraceToken).AddLeadingNewLinesAndSpaces(1, level * LeadingSpacesPerLevel);

            return (openingBrace, closingBrace);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static (SyntaxToken OpeningBracket, SyntaxToken ClosingBracket) GetBrackets(int level)
        {
            var openingBrace = Token(SyntaxKind.OpenBracketToken).AddLeadingNewLinesAndSpaces(1, level * LeadingSpacesPerLevel);
            var closingBrace = Token(SyntaxKind.CloseBracketToken).AddLeadingNewLinesAndSpaces(1, level * LeadingSpacesPerLevel);

            return (openingBrace, closingBrace);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static (SyntaxToken OpeningBracket, SyntaxToken ClosingBracket) GetBrackets()
        {
            var openingBrace = Token(SyntaxKind.OpenBracketToken).AddTrialingSpaces();
            var closingBrace = Token(SyntaxKind.CloseBracketToken).AddLeadingSpaces();

            return (openingBrace, closingBrace);
        }

        private static IReadOnlyCollection<T> GetIndentedNodes<T>(IReadOnlyCollection<T> modifiers, int level, bool lastNodeTrailingLine = false)
            where T : SyntaxNode
        {
            if (modifiers is null)
            {
                return Array.Empty<T>();
            }

            var items = new List<T>(modifiers.Count);

            var i = 0;
            foreach (var modifier in modifiers)
            {
                var addModifier = modifier.AddLeadingNewLinesAndSpaces(1, level * LeadingSpacesPerLevel);
                if (i == modifiers.Count - 1 && lastNodeTrailingLine)
                {
                    addModifier = addModifier.AddTrialingNewLines();
                }

                items.Add(addModifier);

                i++;
            }

            return items;
        }

        private static (SyntaxToken OpenParentheses, SyntaxToken ClosedParentheses) GetParentheses()
        {
            var openingBrace = Token(SyntaxKind.OpenParenToken);
            var closingBrace = Token(SyntaxKind.CloseParenToken).AddTrialingSpaces();

            return (openingBrace, closingBrace);
        }

        private static IReadOnlyCollection<AccessorDeclarationSyntax> GetSpacedAccessors(IReadOnlyCollection<AccessorDeclarationSyntax> accessors)
        {
            var i = 0;

            var items = new List<AccessorDeclarationSyntax>(accessors.Count);

            foreach (var accessor in accessors)
            {
                var returnValue = accessor;
                if (i != accessors.Count - 1)
                {
                    returnValue = returnValue.AddTrialingSpaces();
                }

                items.Add(returnValue);
                i++;
            }

            return items;
        }

        private static SyntaxKind GetAssignmentExpressionOperatorTokenKind(SyntaxKind kind)
        {
            return kind switch
            {
                SyntaxKind.SimpleAssignmentExpression => SyntaxKind.EqualsToken,
                SyntaxKind.AddAssignmentExpression => SyntaxKind.PlusEqualsToken,
                SyntaxKind.SubtractAssignmentExpression => SyntaxKind.MinusEqualsToken,
                SyntaxKind.MultiplyAssignmentExpression => SyntaxKind.AsteriskEqualsToken,
                SyntaxKind.DivideAssignmentExpression => SyntaxKind.SlashEqualsToken,
                SyntaxKind.ModuloAssignmentExpression => SyntaxKind.PercentEqualsToken,
                SyntaxKind.AndAssignmentExpression => SyntaxKind.AmpersandEqualsToken,
                SyntaxKind.ExclusiveOrAssignmentExpression => SyntaxKind.CaretEqualsToken,
                SyntaxKind.OrAssignmentExpression => SyntaxKind.BarEqualsToken,
                SyntaxKind.LeftShiftAssignmentExpression => SyntaxKind.LessThanLessThanEqualsToken,
                SyntaxKind.RightShiftAssignmentExpression => SyntaxKind.GreaterThanGreaterThanEqualsToken,
                SyntaxKind.CoalesceAssignmentExpression => SyntaxKind.QuestionQuestionEqualsToken,
                _ => throw new ArgumentOutOfRangeException(),
            };
        }

        private static void GetTypeValues(string identifier, IReadOnlyCollection<AttributeListSyntax>? attributes, IReadOnlyCollection<SyntaxKind>? modifiers, IReadOnlyCollection<MemberDeclarationSyntax>? members, IReadOnlyCollection<TypeParameterConstraintClauseSyntax>? typeParameterConstraintClauses, IReadOnlyCollection<TypeParameterSyntax>? typeParameters, IReadOnlyCollection<BaseTypeSyntax>? bases, int level, out SyntaxList<AttributeListSyntax> attributeList, out SyntaxToken name, out SyntaxTokenList modifiersList, out BaseListSyntax? baseList, out SyntaxList<MemberDeclarationSyntax> membersList, out TypeParameterListSyntax? typeParameterList, out SyntaxList<TypeParameterConstraintClauseSyntax> typeParameterConstraintList, out SyntaxToken openingBrace, out SyntaxToken closingBrace)
        {
            attributeList = List(attributes, level, true);
            name = Identifier(identifier).AddLeadingSpaces();
            modifiersList = attributes?.Count > 0 ? TokenList(modifiers, level) : TokenList(modifiers);
            baseList = BaseList(bases);
            membersList = List(members, level + 1);
            typeParameterList = TypeParameterList(typeParameters);
            typeParameterConstraintList = typeParameterConstraintClauses?.Count > 0 ? List(typeParameterConstraintClauses) : default;
            (openingBrace, closingBrace) = GetBraces(level);
        }
    }
}
