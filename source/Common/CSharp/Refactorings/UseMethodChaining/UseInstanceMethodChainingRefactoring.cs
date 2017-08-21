// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings.UseMethodChaining
{
    internal class UseInstanceMethodChainingRefactoring : UseMethodChainingRefactoring
    {
        protected override bool IsFixableStatement(
            StatementSyntax statement,
            ExpressionSyntax expression,
            ITypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            if (!statement.IsKind(SyntaxKind.ExpressionStatement))
                return false;

            if (statement.SpanOrLeadingTriviaContainsDirectives())
                return false;

            var expressionStatement = (ExpressionStatementSyntax)statement;

            return MemberInvocationExpression.TryCreate(expressionStatement.Expression, out MemberInvocationExpression memberInvocation)
                && IsFixableInvocation(memberInvocation, typeSymbol, semanticModel, cancellationToken)
                && SyntaxComparer.AreEquivalent(
                    expression,
                    GetFirstInvocationInMethodChain(memberInvocation, typeSymbol, semanticModel, cancellationToken).Expression,
                    requireNotNull: true);
        }

        protected override bool IsFixableInvocation(
            MemberInvocationExpression memberInvocation,
            ITypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            return semanticModel.TryGetMethodInfo(memberInvocation.InvocationExpression, out MethodInfo methodInfo, cancellationToken)
                && !methodInfo.IsExtensionMethod
                && !methodInfo.IsStatic
                && methodInfo.ContainingType?.Equals(typeSymbol) == true
                && methodInfo.ReturnType.Equals(typeSymbol);
        }

        protected override InvocationExpressionSyntax GetInvocationExpression(ExpressionStatementSyntax expressionStatement)
        {
            return expressionStatement.Expression as InvocationExpressionSyntax;
        }
    }
}