using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis.Text;
using System.Threading;

namespace JinShil.CompositionGenerator;

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public class ComposeAttribute : Attribute
{ 

}

[Generator]
internal class CompositionGenerator : IIncrementalGenerator
{

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
#if DEBUG
        if (!Debugger.IsAttached)
        {
            //Debugger.Launch();
        }
#endif  

        var memberDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider
                (
                    static (node, _) => 
                    {
                        if (node is not FieldDeclarationSyntax)
                        {
                            return false;
                        }

                        if (node is not PropertyDeclarationSyntax)
                        {
                            return false;
                        }

                        return true;
                    },
                    static (ctx, token) =>
                    {
                        var member = (MemberDeclarationSyntax)ctx.Node;
                        var semanticModel = ctx.SemanticModel;
                        var symbol = semanticModel.GetDeclaredSymbol(member, token);
                        if (symbol is null)
                        {
                            return null;
                        }

                        var hasComposeAttribute = symbol
                            .GetAttributes()
                            .Any(attr => attr.AttributeClass?.Name is nameof(ComposeAttribute));

                        return member;
                    }
                )
                .Where(a => a is not null);

        context.RegisterSourceOutput(memberDeclarations, (ctx, member) =>
        {

        });
    }
}
