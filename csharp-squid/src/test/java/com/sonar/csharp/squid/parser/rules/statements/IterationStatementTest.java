/*
 * Copyright (C) 2010 SonarSource SA
 * All rights reserved
 * mailto:contact AT sonarsource DOT com
 */
package com.sonar.csharp.squid.parser.rules.statements;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

import com.sonar.csharp.squid.api.CSharpGrammar;
import com.sonar.csharp.squid.parser.CSharpParser;

public class IterationStatementTest {

  CSharpParser p = new CSharpParser();
  CSharpGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.iterationStatement);
    g.whileStatement.mock();
    g.doStatement.mock();
    g.forStatement.mock();
    g.foreachStatement.mock();
  }

  @Test
  public void testOk() {
    assertThat(p, parse("whileStatement"));
    assertThat(p, parse("doStatement"));
    assertThat(p, parse("forStatement"));
    assertThat(p, parse("foreachStatement"));
  }

}
