// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslynator.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings.UseMethodChaining
{
    internal abstract class UseMethodChainingRefactoring
    {
        public static UseInstanceMethodChainingRefactoring InstanceMethod { get; } = new UseInstanceMethodChainingRefactoring();

        public static UseExtensionMethodChainingRefactoring ExtensionMethod { get; } = new UseExtensionMethodChainingRefactoring();

        public static bool IsFixable(
            MemberInvocationExpression memberInvocation,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            InvocationExpressionSyntax invocationExpression = memberInvocation.InvocationExpression;

            SyntaxNode parent = invocationExpression.Parent;

            switch (parent?.Kind())
            {
                case SyntaxKind.ExpressionStatement:
                    {
                        var expressionStatement = (ExpressionStatementSyntax)parent;

                        return InstanceMethod.Analyze(memberInvocation, expressionStatement, semanticModel, cancellationToken);
                    }
                case SyntaxKind.SimpleAssignmentExpression:
                    {
                        var assinmentExpression = (AssignmentExpressionSyntax)parent;

                        if (assinmentExpression.Right != invocationExpression)
                            break;

                        if (!(assinmentExpression.Parent is ExpressionStatementSyntax expressionStatement))
                            break;

                        return ExtensionMethod.Analyze(memberInvocation, expressionStatement, semanticModel, cancellationToken);
                    }
            }

            return false;
        }

        public bool Analyze(
            MemberInvocationExpression memberInvocation,
            StatementSyntax statement,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            if (statement.SpanOrTrailingTriviaContainsDirectives())
                return false;

            if (!StatementContainer.TryCreate(statement, out StatementContainer container))
                return false;

            SyntaxList<StatementSyntax> statements = container.Statements;

            if (statements.Count == 1)
                return false;

            if (!semanticModel.TryGetMethodInfo(memberInvocation.InvocationExpression, out MethodInfo methodInfo, cancellationToken))
                return false;

            ITypeSymbol typeSymbol = methodInfo.ReturnType;

            if (!IsFixableInvocation(memberInvocation, typeSymbol, semanticModel, cancellationToken))
                return false;

            ExpressionSyntax expression = GetFirstInvocationInMethodChain(memberInvocation, typeSymbol, semanticModel, cancellationToken).Expression;

            int i = statements.IndexOf(statement);

            if (i != 0
                && IsFixableStatement(statements[i - 1], expression, typeSymbol, semanticModel, cancellationToken))
            {
                return false;
            }

            int j = i;
            while (j < statements.Count - 1)
            {
                if (!IsFixableStatement(statements[j + 1], expression, typeSymbol, semanticModel, cancellationToken))
                    break;

                j++;
            }

            return j > i;
        }

        protected abstract bool IsFixableStatement(
            StatementSyntax statement,
            ExpressionSyntax expression,
            ITypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken);

        protected abstract bool IsFixableInvocation(
            MemberInvocationExpression memberInvocation,
            ITypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken);

        protected abstract InvocationExpressionSyntax GetInvocationExpression(ExpressionStatementSyntax expressionStatement);

        protected MemberInvocationExpression GetFirstInvocationInMethodChain(
            MemberInvocationExpression memberInvocation,
            ITypeSymbol typeSymbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            while (MemberInvocationExpression.TryCreate(memberInvocation.Expression, out MemberInvocationExpression memberInvocation2)
                && IsFixableInvocation(memberInvocation2, typeSymbol, semanticModel, cancellationToken))
            {
                memberInvocation = memberInvocation2;
            }

            return memberInvocation;
        }

        public async Task<Document> RefactorAsync(
            Document document,
            ExpressionStatementSyntax expressionStatement,
            CancellationToken cancellationToken)
        {
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            InvocationExpressionSyntax invocationExpression = GetInvocationExpression(expressionStatement);

            semanticModel.TryGetMethodInfo(invocationExpression, out MethodInfo methodInfo, cancellationToken);

            MemberInvocationExpression memberInvocation = MemberInvocationExpression.Create(invocationExpression);

            ITypeSymbol typeSymbol = methodInfo.ReturnType;

            ExpressionSyntax expression = GetFirstInvocationInMethodChain(memberInvocation, typeSymbol, semanticModel, cancellationToken).Expression;

            StatementContainer statementContainer = StatementContainer.Create(expressionStatement);

            SyntaxList<StatementSyntax> statements = statementContainer.Statements;

            int index = statements.IndexOf(expressionStatement);

            string indentation = CSharpFormatter.GetIncreasedIndentation(expressionStatement, cancellationToken).ToString();

            var sb = new StringBuilder(invocationExpression.ToString());

            int j = index;
            while (j < statements.Count - 1)
            {
                StatementSyntax statement = statements[j + 1];

                if (!IsFixableStatement(statement, expression, typeSymbol, semanticModel, cancellationToken))
                    break;

                sb.AppendLine();
                sb.Append(indentation);
                sb.Append(GetTextToAppend((ExpressionStatementSyntax)statement, typeSymbol, semanticModel, cancellationToken));

                j++;
            }

            StatementSyntax lastStatement = statements[j];

            SyntaxList<StatementSyntax> newStatements = statements;

            while (j > index)
            {
                newStatements = newStatements.RemoveAt(j);
                j--;
            }

            ExpressionSyntax newInvocationExpression = SyntaxFactory.ParseExpression(sb.ToString());

            SyntaxTriviaList trailingTrivia = statementContainer
                .Node
                .DescendantTrivia(TextSpan.FromBounds(invocationExpression.Span.End, lastStatement.Span.End))
                .ToSyntaxTriviaList()
                .EmptyIfWhitespace()
                .AddRange(lastStatement.GetTrailingTrivia());

            ExpressionStatementSyntax newExpressionStatement = expressionStatement
                .ReplaceNode(invocationExpression, newInvocationExpression)
                .WithLeadingTrivia(expressionStatement.GetLeadingTrivia())
                .WithTrailingTrivia(trailingTrivia)
                .WithFormatterAndSimplifierAnnotations();

            newStatements = newStatements.ReplaceAt(index, newExpressionStatement);

            return await document.ReplaceNodeAsync(statementContainer.Node, statementContainer.NodeWithStatements(newStatements), cancellationToken).ConfigureAwait(false);
        }

        private string GetTextToAppend(
            ExpressionStatementSyntax expressionStatement,
            ITypeSymbol symbol,
            SemanticModel semanticModel,
            CancellationToken cancellationToken)
        {
            MemberInvocationExpression memberInvocation = MemberInvocationExpression.Create(GetInvocationExpression(expressionStatement));

            MemberInvocationExpression firstMemberInvocation = GetFirstInvocationInMethodChain(memberInvocation, symbol, semanticModel, cancellationToken);

            InvocationExpressionSyntax invocationExpression = memberInvocation.InvocationExpression;

            return invocationExpression
                .ToString()
                .Substring(firstMemberInvocation.OperatorToken.SpanStart - invocationExpression.SpanStart);
        }
    }
}