﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Roslynator.CSharp.Refactorings;

namespace Roslynator.CSharp.CodeFixProviders
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(RemoveRedundantPartialModifierCodeFixProvider))]
    [Shared]
    public class RemoveRedundantPartialModifierCodeFixProvider : BaseCodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(DiagnosticIdentifiers.RemovePartialModifierFromTypeWithSinglePart);

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            SyntaxNode root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            SyntaxNode declaration = root
                .FindNode(context.Span, getInnermostNodeForTie: true)?
                .FirstAncestorOrSelf(
                    SyntaxKind.ClassDeclaration,
                    SyntaxKind.StructDeclaration,
                    SyntaxKind.InterfaceDeclaration);

            if (declaration == null)
                return;

            CodeAction codeAction = CodeAction.Create(
                "Remove 'partial' modifier",
                cancellationToken => RemoveModifierRefactoring.RemovePartialModifierAsync(context.Document, declaration, cancellationToken),
                DiagnosticIdentifiers.RemovePartialModifierFromTypeWithSinglePart + EquivalenceKeySuffix);

            context.RegisterCodeFix(codeAction, context.Diagnostics);
        }
    }
}
