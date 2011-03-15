/*
 * Copyright (C) 2010 SonarSource SA
 * All rights reserved
 * mailto:contact AT sonarsource DOT com
 */
package com.sonar.csharp.squid.parser.rules.classes;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

import com.sonar.csharp.squid.api.CSharpGrammar;
import com.sonar.csharp.squid.parser.CSharpParser;

public class FormalParameterListTest {

  CSharpParser p = new CSharpParser();
  CSharpGrammar g = p.getGrammar();

  @Before
  public void init() {
    p.setRootRule(g.formalParameterList);
  }

  @Test
  public void testOk() {
    g.fixedParameters.mock();
    g.parameterArray.mock();
    assertThat(p, parse("fixedParameters"));
    assertThat(p, parse("parameterArray"));
    assertThat(p, parse("fixedParameters, parameterArray"));
  }

  @Test
  public void testRealLife() throws Exception {
    assertThat(p, parse("int i"));
    assertThat(p, parse("this IEnumerable<TSource> source, Func<TSource, int, bool> predicate"));
    assertThat(p, parse("RequestStatusDto? status, UserActionDto? action, OTCTypeDto? dealType"));
  }

}
