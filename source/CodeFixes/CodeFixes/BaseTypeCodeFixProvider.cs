﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Roslynator.CSharp.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(BaseTypeCodeFixProvider))]
    [Shared]
    public class BaseTypeCodeFixProvider : BaseCodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(CompilerDiagnosticIdentifiers.UsingGenericTypeRequiresTypeArguments); }
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            if (!Settings.IsCodeFixEnabled(CodeFixIdentifiers.AddTypeArgument))
                return;

            SyntaxNode root = await context.GetSyntaxRootAsync().ConfigureAwait(false);

            TypeSyntax type = root
                .FindNode(context.Span, getInnermostNodeForTie: true)?
                .FirstAncestorOrSelf<TypeSyntax>();

            Debug.Assert(type != null, $"{nameof(type)} is null");

            if (type == null)
                return;

            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                switch (diagnostic.Id)
                {
                    case CompilerDiagnosticIdentifiers.UsingGenericTypeRequiresTypeArguments:
                        {
                            SemanticModel semanticModel = await context.GetSemanticModelAsync().ConfigureAwait(false);

                            SymbolInfo symbolInfo = semanticModel.GetSymbolInfo(type, context.CancellationToken);

                            foreach (ISymbol symbol in symbolInfo.CandidateSymbols)
                            {
                                var namedTypeSymbol = symbol as INamedTypeSymbol;

                                if (namedTypeSymbol != null)
                                {
                                    ImmutableArray<ITypeParameterSymbol> typeParameters = namedTypeSymbol.TypeParameters;

                                    if (typeParameters.Any())
                                    {
                                        CodeAction codeAction = CodeAction.Create(
                                            GetTitle(typeParameters),
                                            cancellationToken =>
                                            {
                                                SeparatedSyntaxList<TypeSyntax> typeArguments = CreateTypeArguments(typeParameters, type.SpanStart, semanticModel).ToSeparatedSyntaxList();

                                                var identifierName = (IdentifierNameSyntax)type;

                                                GenericNameSyntax newNode = SyntaxFactory.GenericName(identifierName.Identifier, SyntaxFactory.TypeArgumentList(typeArguments));

                                                return context.Document.ReplaceNodeAsync(type, newNode, context.CancellationToken);
                                            },
                                            GetEquivalenceKey(diagnostic, SymbolDisplay.GetString(namedTypeSymbol)));

                                        context.RegisterCodeFix(codeAction, diagnostic);
                                    }
                                }
                            }

                            break;
                        }
                }
            }
        }

        private static string GetTitle(ImmutableArray<ITypeParameterSymbol> typeParameters)
        {
            if (typeParameters.Length == 1)
            {
                return $"Add type argument {typeParameters[0].Name}";
            }
            else
            {
                return $"Add type arguments {string.Join(", ", typeParameters.Select(f => f.Name))}";
            }
        }

        private static string GetTitle(ref ImmutableArray<ITypeParameterSymbol> typeParameters)
        {
            return (typeParameters.Length == 1) ? "Add type argument" : "Add type arguments";
        }

        private static IEnumerable<TypeSyntax> CreateTypeArguments(
            ImmutableArray<ITypeParameterSymbol> typeParameters,
            int position,
            SemanticModel semanticModel)
        {
            bool isFirst = true;

            ImmutableArray<ISymbol> symbols = semanticModel.LookupSymbols(position);

            foreach (ITypeParameterSymbol typeParameter in typeParameters)
            {
                string name = typeParameter.Name;

                name = NameGenerator.Default.EnsureUniqueName(
                    typeParameter.Name,
                    symbols);

                SyntaxToken identifier = SyntaxFactory.Identifier(name);

                if (isFirst)
                {
                    identifier = identifier.WithRenameAnnotation();
                    isFirst = false;
                }

                yield return SyntaxFactory.IdentifierName(identifier);
            }
        }
    }
}
