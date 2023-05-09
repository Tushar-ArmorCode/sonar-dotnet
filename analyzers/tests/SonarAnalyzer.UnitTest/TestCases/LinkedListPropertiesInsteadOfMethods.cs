﻿using System.Collections;
using System;
using System.Collections.Generic;
using System.Linq;

class MyClass
{
    void Various()
    {
        var data = new LinkedList<int>();

        data.First(); // Noncompliant {{'First' property of 'LinkedList' should be used instead of the 'First()' extension method.}}
//           ^^^^^
        data.Last(); // Noncompliant {{'Last' property of 'LinkedList' should be used instead of the 'Last()' extension method.}}
//           ^^^^
        data.First(x => x > 0);            // Compliant
        data.Last(x => x > 0);             // Compliant
        _ = data.First.Value;              // Compliant
        _ = data.Last.Value;               // Compliant
        data.Count();                      // Compliant
        data.Append(1).First().ToString(); // Compliant
        data.Append(1).Last().ToString();  // Compliant
        data?.First().ToString();          // Noncompliant
        data?.Last().ToString();           // Noncompliant

        var classA = new ClassA();
        classA.myLinkedListField.First();  // Noncompliant
        classA.classB.myLinkedListField.First(); // Noncompliant
        classA.classB.First(); // Compliant

        data?.First();            // Noncompliant
        data?.First().ToString(); // Noncompliant
        data?.First(x => x > 0);  // Compliant

        var enumData = new EnumData<int>();
        enumData.First(); // Compliant

        var goodLinkedList = new GoodLinkedList<int>();
        goodLinkedList.First(); // Noncompliant

        var ternary = (true ? data : goodLinkedList).First(); // Noncompliant
        var nullCoalesce = (data ?? goodLinkedList).First();  // Noncompliant
        var ternaryNullCoalesce = (data ?? (true ? data : goodLinkedList)).First(); // Noncompliant

        goodLinkedList.GetLinkedList().GetLinkedList().GetLinkedList().GetLinkedList().First();     //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList().GetLinkedList().GetLinkedList()?.First();    //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList().GetLinkedList()?.GetLinkedList().First();    //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList().GetLinkedList()?.GetLinkedList()?.First();   //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList()?.GetLinkedList().GetLinkedList().First();    //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList()?.GetLinkedList().GetLinkedList()?.First();   //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList()?.GetLinkedList()?.GetLinkedList().First();   //Noncompliant
        goodLinkedList.GetLinkedList().GetLinkedList()?.GetLinkedList()?.GetLinkedList()?.First();  //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList().GetLinkedList().GetLinkedList().First();    //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList().GetLinkedList().GetLinkedList()?.First();   //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList().GetLinkedList()?.GetLinkedList().First();   //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList().GetLinkedList()?.GetLinkedList()?.First();  //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList()?.GetLinkedList().GetLinkedList().First();   //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList()?.GetLinkedList().GetLinkedList()?.First();  //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList()?.GetLinkedList()?.GetLinkedList().First();  //Noncompliant
        goodLinkedList.GetLinkedList()?.GetLinkedList()?.GetLinkedList()?.GetLinkedList()?.First(); //Noncompliant
//                                                                                         ^^^^^
    }

    int GetFirst(LinkedList<int> data) => data.First(); // Noncompliant

    class GoodLinkedList<T> : LinkedList<T>
    {
        public GoodLinkedList<T> GetLinkedList() => this;
        T CallFirst() => this.First(); // Noncompliant
    }

    class EnumData<T> : IEnumerable<T>
    {
        public IEnumerator<T> GetEnumerator() => null;
        IEnumerator IEnumerable.GetEnumerator() => null;
    }

    class ClassA
    {
        public LinkedList<int> myLinkedListField = new LinkedList<int>();

        public LinkedList<int> myLinkedListProperty
        {
            get => myLinkedListField;
            set => myLinkedListField.AddLast(value.First()); // Noncompliant
        }

        public ClassB classB = new ClassB();
    }
}

public class ClassB
{
    public LinkedList<int> myLinkedListField = new LinkedList<int>();

    public int First() => 0;
}
