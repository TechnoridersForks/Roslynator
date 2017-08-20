// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using static Roslynator.CSharp.GenericDeclarationHelper;

namespace Roslynator.CSharp.Refactorings
{
    internal static class ReorderTypeParameterConstraintsRefactoring
    {
        public static void AnalyzeTypeParameterList(SyntaxNodeAnalysisContext context)
        {
            var typeParameterList = (TypeParameterListSyntax)context.Node;

            SeparatedSyntaxList<TypeParameterSyntax> typeParameters = typeParameterList.Parameters;

            if (!typeParameters.Any())
                return;

            SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses = GetConstraintClauses(typeParameterList.Parent);

            if (!constraintClauses.Any())
                return;

            if (constraintClauses.SpanContainsDirectives())
                return;

            if (!IsFixable(typeParameters, constraintClauses))
                return;

            context.ReportDiagnostic(
                DiagnosticDescriptors.ReorderTypeParameterConstraints,
                constraintClauses.First());
        }

        private static bool IsFixable(
            SeparatedSyntaxList<TypeParameterSyntax> typeParameters,
            SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses)
        {
            int lastIndex = -1;

            for (int i = 0; i < typeParameters.Count; i++)
            {
                string name = typeParameters[i].Identifier.ValueText;

                int index = IndexOf(constraintClauses, name);

                if (index != -1)
                {
                    if (index < lastIndex)
                        return true;

                    lastIndex = index;
                }
            }

            return false;
        }

        private static int IndexOf(SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses, string name)
        {
            for (int i = 0; i < constraintClauses.Count; i++)
            {
                if (constraintClauses[i].Name.Identifier.ValueText == name)
                    return i;
            }

            return -1;
        }

        public static Task<Document> RefactorAsync(
            Document document,
            SyntaxNode node,
            CancellationToken cancellationToken)
        {
            SyntaxList<TypeParameterConstraintClauseSyntax> newConstraintClauses = SortConstraints(GetTypeParameterList(node).Parameters, GetConstraintClauses(node));

            SyntaxNode newNode = WithConstraintClauses(node, newConstraintClauses);

            return document.ReplaceNodeAsync(node, newNode, cancellationToken);
        }

        private static SyntaxList<TypeParameterConstraintClauseSyntax> SortConstraints(
            SeparatedSyntaxList<TypeParameterSyntax> typeParameters,
            SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses)
        {
            int lastIndex = -1;

            for (int i = 0; i < typeParameters.Count; i++)
            {
                string name = typeParameters[i].Identifier.ValueText;

                int index = IndexOf(constraintClauses, name);

                if (index != -1)
                {
                    if (index != lastIndex + 1)
                        constraintClauses = constraintClauses.Swap(index, lastIndex + 1);

                    lastIndex++;
                }
            }

            return constraintClauses;
        }
    }
}
