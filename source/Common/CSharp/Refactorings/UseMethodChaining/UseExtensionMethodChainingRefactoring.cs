// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings.UseMethodChaining
{
    internal class UseExtensionMethodChainingRefactoring : UseMethodChainingRefactoring
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

            if (!SimpleAssignmentExpression.TryCreate(expressionStatement.Expression, out SimpleAssignmentExpression simpleAssignment))
                return false;

            return MemberInvocationExpression.TryCreate(simpleAssignment.Right, out MemberInvocationExpression memberInvocation)
                && SyntaxComparer.AreEquivalent(simpleAssignment.Left, memberInvocation.Expression)
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
                && methodInfo.IsExtensionMethod
                && methodInfo.Symbol.ReducedFrom?.Parameters[0].Type.Equals(typeSymbol) == true
                && methodInfo.ReturnType.Equals(typeSymbol);
        }

        protected override InvocationExpressionSyntax GetInvocationExpression(ExpressionStatementSyntax expressionStatement)
        {
            if (!(expressionStatement.Expression is AssignmentExpressionSyntax assignmentExpression))
                return null;

            if (assignmentExpression.Kind() != SyntaxKind.SimpleAssignmentExpression)
                return null;

            return assignmentExpression.Right as InvocationExpressionSyntax;
        }
    }
}