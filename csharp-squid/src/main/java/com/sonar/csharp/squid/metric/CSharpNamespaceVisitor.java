/*
 * Copyright (C) 2010 SonarSource SA
 * All rights reserved
 * mailto:contact AT sonarsource DOT com
 */
package com.sonar.csharp.squid.metric;

import org.sonar.plugins.csharp.api.CSharpMetric;

import com.sonar.csharp.squid.api.ast.CSharpAstVisitor;
import com.sonar.sslr.api.AstNode;

/**
 * Visitor that creates namespace (<=>package) resources and computes the number of namespace.
 */
public class CSharpNamespaceVisitor extends CSharpAstVisitor {

  /**
   * {@inheritDoc}
   */
  @Override
  public void init() {
    subscribeTo(getCSharpGrammar().namespaceDeclaration);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void visitNode(AstNode astNode) {
    getProject().setMeasure(CSharpMetric.NAMESPACES, 1);
  }

}
