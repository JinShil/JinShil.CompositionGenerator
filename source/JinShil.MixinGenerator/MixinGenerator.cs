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

namespace JinShil.MixinGenerator;

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public sealed class MixinAttribute : Attribute
{

}

[Generator]
internal class MixinGenerator : IIncrementalGenerator
{
    static bool HasAttribute(SyntaxNode node)
    {
        if (node is not MemberDeclarationSyntax memberNode)
        {
            return false;
        }

        const string attribute = nameof(Attribute);
        var attributeName = nameof(MixinAttribute).Remove(nameof(MixinAttribute).LastIndexOf(attribute));

        var attributes = memberNode.AttributeLists;
        foreach(var a in attributes)
        {
            foreach (var attr in a.Attributes)
            {
                var name = attr.Name.ToString();
                if (name == nameof(MixinAttribute) || name == attributeName)
                {
                    return true;
                }
            }
        }

        return false;
    }

    // Groups ISymbol instances (e.g., fields, properties) by their containing class or struct
    static Dictionary<INamedTypeSymbol, List<ISymbol>> GroupMembersByContainingType(IEnumerable<ISymbol> members)
    {
        // Initialize the result dictionary
        // Key: INamedTypeSymbol of the containing class or struct
        // Value: List of ISymbol instances (e.g., IFieldSymbol, IPropertySymbol)
        var groups = new Dictionary<INamedTypeSymbol, List<ISymbol>>(SymbolEqualityComparer.Default);

        foreach (var memberSymbol in members)
        {
            if (memberSymbol == null)
            {
                continue; // Skip if member is null
            }

            // Step 1: Get the containing type from the symbol
            var containingType = memberSymbol.ContainingType;

            if (containingType == null)
            {
                continue; // Skip if member is not in a type (e.g., global field)
            }

            // Step 2: Filter for classes or structs
            if (containingType.TypeKind != TypeKind.Class && containingType.TypeKind != TypeKind.Struct)
            {
                continue; // Skip if not a class or struct (e.g., interface, enum)
            }

            // Step 3: Add the member symbol to the group
            if (!groups.TryGetValue(containingType, out var memberList))
            {
                // Initialize a new list for this type
                memberList = [];
                groups[containingType] = memberList;
            }
            
            memberList.Add(memberSymbol);
        }

        return groups;
    }

    static IEnumerable<ISymbol> GetPublicMembers(ITypeSymbol type)
    {
        var current = type;
        var seen = new HashSet<string>();

        while (current != null
            && current.SpecialType != SpecialType.System_Object)  // Don't include members from `object`.
        {
            foreach (var member in current.GetMembers())
            {
                if (member.DeclaredAccessibility == Accessibility.Public
                    && !member.IsStatic
                    && seen.Add(member.Name))
                {
                    if (member is IMethodSymbol method)
                    {
                        // Only ordinary methods.  No constructors, destructors, etc.
                        if (method.MethodKind != MethodKind.Ordinary && method.MethodKind != MethodKind.ExplicitInterfaceImplementation)
                        {
                            continue;
                        }
                    }
                    
                    yield return member;
                }
            }

            current = current.BaseType;
        }
    }

    static IEnumerable<MemberDeclarationSyntax> GenerateWrappers(ISymbol memberToMixin, ITypeSymbol memberType)
    {
        foreach(var member in GetPublicMembers(memberType))
        {
            if (member is IFieldSymbol field)
            {
                // Generate a property for the field
            }
            else if (member is IPropertySymbol property)
            {
                var isExplicit = property.ExplicitInterfaceImplementations.Any();
                var type = SyntaxFactory.ParseTypeName(property.Type.ToDisplayString());
                var name = property.Name;

                var declaration  = SyntaxFactory.PropertyDeclaration(type, name);
                if (!isExplicit)
                {
                    // Add public access modifier
                    declaration = declaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
                }
                else
                {
                    // Add explicit interface specifier
                    var interfaceType = property.ExplicitInterfaceImplementations.First().ContainingType;
                    var interfaceName = SyntaxFactory.ParseName(interfaceType.ToDisplayString());
                    var explicitSpecifier = SyntaxFactory.ExplicitInterfaceSpecifier(interfaceName);
                    declaration = declaration.WithExplicitInterfaceSpecifier(explicitSpecifier);
                }

                var accessors = new List<AccessorDeclarationSyntax>();

                // create getter
                if (property.GetMethod != null)
                {
                    var propertyInvocation = SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(memberToMixin.Name),
                        SyntaxFactory.IdentifierName(name));
                    var body = SyntaxFactory.Block(SyntaxFactory.ReturnStatement(propertyInvocation));
                    var getter = SyntaxFactory.AccessorDeclaration(
                        SyntaxKind.GetAccessorDeclaration,
                        body);
                    accessors.Add(getter);
                }

                // create setter
                if (property.SetMethod != null)
                {
                    var propertyInvocation = SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(memberToMixin.Name),
                        SyntaxFactory.IdentifierName(name));
                    var assignment = SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName("value"),
                        propertyInvocation);
                    var body = SyntaxFactory.Block(SyntaxFactory.ExpressionStatement(assignment));
                    var setter = SyntaxFactory.AccessorDeclaration(
                        SyntaxKind.SetAccessorDeclaration, 
                        body);
                    accessors.Add(setter);
                }

                declaration = declaration.AddAccessorListAccessors([.. accessors]);

                yield return declaration;

            }
            else if (member is IMethodSymbol method)
            {
                var isExplicit = method.ExplicitInterfaceImplementations.Any();
                var type = SyntaxFactory.ParseTypeName(method.ReturnType.ToDisplayString());
                var name = method.Name;

                var declaration = SyntaxFactory.MethodDeclaration(type, name);
                if (!isExplicit)
                {
                    // Add public access modifier
                    declaration = declaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
                }
                else
                {
                    // Add explicit interface specifier
                    var interfaceType = method.ExplicitInterfaceImplementations.First().ContainingType;
                    var interfaceName = SyntaxFactory.ParseName(interfaceType.ToDisplayString());
                    var explicitSpecifier = SyntaxFactory.ExplicitInterfaceSpecifier(interfaceName);
                    declaration = declaration.WithExplicitInterfaceSpecifier(explicitSpecifier);
                }

                // Add parameters
                foreach (var parameter in method.Parameters)
                {
                    var paramType = SyntaxFactory.ParseTypeName(parameter.Type.ToDisplayString());
                    var paramName = parameter.Name;
                    var paramSyntax = SyntaxFactory.Parameter(SyntaxFactory.Identifier(paramName))
                        .WithType(paramType);

                    // Add ref, out, in, etc...
                    if (parameter.RefKind != RefKind.None)
                    {
                        var token = SyntaxFactory.Token(SyntaxKind.None);
                        switch (parameter.RefKind)
                        {
                            case RefKind.None:
                                break;
                            case RefKind.Ref:
                                token = SyntaxFactory.Token(SyntaxKind.RefKeyword);
                                break;
                            case RefKind.Out:
                                token = SyntaxFactory.Token(SyntaxKind.OutKeyword);
                                break;
                            case RefKind.In:
                                token = SyntaxFactory.Token(SyntaxKind.InKeyword);
                                break;
                        }

                        declaration = declaration.AddModifiers(token);
                    }

                    declaration = declaration.AddParameterListParameters(paramSyntax);
                }

                // Add return statement
                if (method.ReturnsVoid)
                {
                    declaration = declaration.WithBody(SyntaxFactory.Block());
                }
                else
                {
                    var returnStatement = SyntaxFactory.ReturnStatement(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.ThisExpression(),
                            SyntaxFactory.IdentifierName(memberToMixin.Name)));
                    declaration = declaration.WithBody(SyntaxFactory.Block(returnStatement));
                }
                
                yield return declaration;
            }
            else if (member is IEventSymbol eventSymbol)
            {
                var isExplicit = eventSymbol.ExplicitInterfaceImplementations.Any();
                var type = SyntaxFactory.ParseTypeName(eventSymbol.Type.ToDisplayString());
                var name = eventSymbol.Name;
                var declaration = SyntaxFactory.EventDeclaration(type, name);
                if (!isExplicit)
                {
                    // Add public access modifier
                    declaration = declaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
                }
                else
                {
                    // Add explicit interface specifier
                    var interfaceType = eventSymbol.ExplicitInterfaceImplementations.First().ContainingType;
                    var interfaceName = SyntaxFactory.ParseName(interfaceType.ToDisplayString());
                    var explicitSpecifier = SyntaxFactory.ExplicitInterfaceSpecifier(interfaceName);
                    declaration = declaration.WithExplicitInterfaceSpecifier(explicitSpecifier);
                }

                // Add the event accessors
                var accessors = new List<AccessorDeclarationSyntax>();

                // create add accessor
                if (eventSymbol.AddMethod != null)
                {
                    var eventInvocation = SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(memberToMixin.Name),
                        SyntaxFactory.IdentifierName(name));
                    var body = SyntaxFactory.Block(SyntaxFactory.ExpressionStatement(eventInvocation));
                    var addAccessor = SyntaxFactory.AccessorDeclaration(
                        SyntaxKind.AddAccessorDeclaration,
                        body);
                    accessors.Add(addAccessor);
                }

                // create remove accessor
                if (eventSymbol.RemoveMethod != null)
                {
                    var eventInvocation = SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(memberToMixin.Name),
                        SyntaxFactory.IdentifierName(name));
                    var body = SyntaxFactory.Block(SyntaxFactory.ExpressionStatement(eventInvocation));
                    var removeAccessor = SyntaxFactory.AccessorDeclaration(
                        SyntaxKind.RemoveAccessorDeclaration,
                        body);
                    accessors.Add(removeAccessor);
                }

                declaration = declaration.AddAccessorListAccessors([.. accessors]);

                yield return declaration;
            }
        }
    }

    static IEnumerable<MemberDeclarationSyntax> GenerateComposedMembers(INamedTypeSymbol containingType, IEnumerable<ISymbol> members)
    {
        foreach(var member in members)
        {
            if (member is IFieldSymbol field)
            {
                foreach (var wrapper in GenerateWrappers(field, field.Type))
                {
                    yield return wrapper;
                }
            }
            else if (member is IPropertySymbol property)
            {
                foreach (var wrapper in GenerateWrappers(property, property.Type))
                {
                    yield return wrapper;
                }
            }
        }
    }

    public static void GetGenericTypesAndConstraints(
        IEnumerable<ITypeParameterSymbol> typeParameters, 
        out IEnumerable<TypeParameterSyntax> genericTypes,
        out IReadOnlyDictionary<string, TypeParameterConstraintClauseSyntax> whereClauses)
    {
        var gt = new List<TypeParameterSyntax>();
        var wc = new Dictionary<string, TypeParameterConstraintClauseSyntax>();
        foreach (var t in typeParameters)
        {
            gt.Add(SyntaxFactory.TypeParameter(t.ToDisplayString()));

            
            var typeConstraints = new List<TypeParameterConstraintSyntax>();
            foreach (var c in t.ConstraintTypes)
            {
                var typeName = SyntaxFactory.ParseTypeName(c.ToDisplayString());
                var tc = SyntaxFactory.TypeConstraint(typeName);
                typeConstraints.Add(tc);
            }

            if (t.HasReferenceTypeConstraint)
            {
                var tc = SyntaxFactory.ClassOrStructConstraint(SyntaxKind.ClassConstraint);
                typeConstraints.Add(tc);
            }

            if (t.HasValueTypeConstraint)
            {
                var tc = SyntaxFactory.ClassOrStructConstraint(SyntaxKind.StructConstraint);
                typeConstraints.Add(tc);
            }

            if (typeConstraints.Any())
            {
                var typeId = SyntaxFactory.IdentifierName(t.ToDisplayString());
                var clause = SyntaxFactory.TypeParameterConstraintClause(typeId, SyntaxFactory.SeparatedList(typeConstraints));
                wc.Add(t.ToDisplayString(), clause);
            }
        }

        genericTypes = gt;
        whereClauses = wc;
    }

    static ClassDeclarationSyntax GeneratePartialClass(INamedTypeSymbol containingType, IEnumerable<ISymbol> members)
    {
        // Get where clauses for containing type
        GetGenericTypesAndConstraints(
            containingType.TypeParameters,
            out var genericTypes,
            out var whereClauses);

        var declaration = SyntaxFactory.ClassDeclaration(containingType.Name);
        declaration = declaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        declaration = declaration.AddModifiers(SyntaxFactory.Token(SyntaxKind.PartialKeyword));
        
         // Add the generic type parameters if there are any
        if (containingType.IsGenericType)
        {
            var typeParameterList = SyntaxFactory.SeparatedList(genericTypes);
            declaration = declaration.AddTypeParameterListParameters([.. typeParameterList]);
        }

        // Add generic type constraints
        foreach(var w in whereClauses)
        {
            declaration = declaration.AddConstraintClauses(w.Value);
        }

        // Add generic type constraints
        foreach(var w in whereClauses)
        {
            declaration = declaration.AddConstraintClauses(w.Value);
        }
        
        // Add the members to the class
        var composedMembers = GenerateComposedMembers(containingType, members);
        declaration = declaration.AddMembers(composedMembers.ToArray());

        return declaration;
    }

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
#if DEBUG
        if (!Debugger.IsAttached)
        {
            Debugger.Launch();
        }
#endif   

        var memberDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider
                (
                    static (node, _) => HasAttribute(node),
                    static (ctx, _) => 
                    {
                        if (ctx.Node is FieldDeclarationSyntax f)
                        {
                            return ctx.SemanticModel.GetDeclaredSymbol(f.Declaration.Variables.First());
                        }

                        return ctx.SemanticModel.GetDeclaredSymbol(ctx.Node);
                    }
                );

        context.RegisterSourceOutput(
            context.CompilationProvider.Combine(memberDeclarations.Collect()),
            (spc, source) =>
            {
                var compilation = source.Left;
                var symbols = source.Right;

                // Group the symbols by their containing type
                var symbolsByType = GroupMembersByContainingType(symbols);

                // Generate the parial class/struct for each containing type containing the composed members
                foreach (var kvp in symbolsByType)
                {
                    var containingType = kvp.Key;
                    var members = kvp.Value;

                    // start by declaring the namespace
                    var namepaceDeclaration = SyntaxFactory.NamespaceDeclaration(
                        SyntaxFactory.ParseName(containingType.ContainingNamespace.ToDisplayString()));

                    // Add the class declaration to the namespace
                    var classDeclaration = GeneratePartialClass(containingType, members);
                    namepaceDeclaration = namepaceDeclaration.AddMembers(classDeclaration);

                    // Add the namespace to the compilation unit
                    var compilationUnit = SyntaxFactory.CompilationUnit()
                        .AddMembers(namepaceDeclaration);

                    // generate the source code
                    var sourceText = compilationUnit.NormalizeWhitespace().ToFullString();

                    // Create a unique filename for the generated source file
                    string filename = containingType.ToDisplayString(
                        new SymbolDisplayFormat(
                            typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces
                        )
                    );

                    // add the source code to the file
                    spc.AddSource($"{filename}.g.cs", sourceText);
                }
            });
    }
}
