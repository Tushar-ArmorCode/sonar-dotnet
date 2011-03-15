/*
 * Copyright (C) 2010 SonarSource SA
 * All rights reserved
 * mailto:contact AT sonarsource DOT com
 */
package com.sonar.csharp.squid.parser.rules.expressions;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

import com.sonar.csharp.squid.api.CSharpGrammar;
import com.sonar.csharp.squid.parser.CSharpParser;

public class PrimaryExpressionTest {

  CSharpParser p = new CSharpParser();
  CSharpGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.primaryExpression);
  }

  @Test
  public void testOk() {
    g.arrayCreationExpression.mock();
    g.primaryNoArrayCreationExpression.mock();
    assertThat(p, parse("arrayCreationExpression"));
    assertThat(p, parse("primaryNoArrayCreationExpression"));
  }

  @Test
  public void testRealLife() throws Exception {
    assertThat(p, parse("typeof(void).OneAttribute"));
    assertThat(p, parse("CurrentDomain.GetAssemblies()"));
    assertThat(p, parse("dbCommand.Dispose()"));
    assertThat(p, parse("checked(++i)"));
  }

}
