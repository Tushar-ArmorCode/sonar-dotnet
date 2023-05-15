﻿Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Linq

Public Class TestClass
    Private Sub MyMethod(ByVal list As List(Of Integer), ByVal array As Integer())
        list.Any(Function(x) x = 0) ' Noncompliant {{Collection-specific "Contains" method should be used instead of the "Any" extension.}}
        '    ^^^

        list.Any(Function(x) x > 0) ' Compliant
        list.Append(1).Any(Function(x) x = 1) ' Compliant (Appended list becomes an IEnumerable)
        list.Append(1).Append(2).Any(Function(x) x = 1) ' Compliant
        Enumerable.Any(Enumerable.Append(Enumerable.Append(list, 1), 2), Function(x) x = 1).ToString() ' Compliant 

        list.Any() ' Compliant
        list.Contains(0) ' Compliant

        Dim classA = New ClassA()
        classA.myListField.Any(Function(x) x = 0) ' Noncompliant
        classA.classB.myListField.Any(Function(x) x = 0) ' Noncompliant
        classA.classB.myListField.Any() ' Compliant

        Dim classB = New ClassB()
        classB.Any(Function(x) x = 0) ' Compliant

        list?.Any(Function(x) x = 0) ' Noncompliant
        classB?.Any(Function(x) x = 0) ' Compliant

        Dim del As Func(Of Integer, Boolean) = Function(x) True
        list.Any(del) ' Compliant

        Dim enumList = New EnumList(Of Integer)()
        enumList.Any(Function(x) x = 0) ' Compliant

        Dim goodList = New GoodList(Of Integer)()
        goodList.Any(Function(x) x = 0) ' Noncompliant

        Dim ternary = If(True, list, goodList).Any(Function(x) x = 0) ' Noncompliant
        Dim nullCoalesce = If(list, goodList).Any(Function(x) x = 0) ' Noncompliant
        Dim ternaryNullCoalesce = If(list, If(True, list, goodList)).Any(Function(x) x = 0) ' Noncompliant

        goodList.GetList().Any(Function(x) True) ' Compliant

        Any(Of Integer)(Function(x) x = 0) ' Compliant
        Call AcceptMethod(New Func(Of Func(Of Integer, Boolean), Boolean)(AddressOf goodList.Any)) ' Compliant
    End Sub

    Private Sub ConditionalsMatrix(ByVal goodList As GoodList(Of Integer))
        goodList.GetList().GetList().GetList().GetList().Any(Function(x) x = 0)     ' Noncompliant
        goodList.GetList().GetList().GetList().GetList()?.Any(Function(x) x = 0)    ' Noncompliant
        goodList.GetList().GetList().GetList()?.GetList().Any(Function(x) x = 0)    ' Noncompliant
        goodList.GetList().GetList().GetList()?.GetList()?.Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList().GetList()?.GetList().GetList().Any(Function(x) x = 0)    ' Noncompliant
        goodList.GetList().GetList()?.GetList().GetList()?.Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList().GetList()?.GetList()?.GetList().Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList().GetList()?.GetList()?.GetList()?.Any(Function(x) x = 0)  ' Noncompliant
        goodList.GetList()?.GetList().GetList().GetList().Any(Function(x) x = 0)    ' Noncompliant
        goodList.GetList()?.GetList().GetList().GetList()?.Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList()?.GetList().GetList()?.GetList().Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList()?.GetList().GetList()?.GetList()?.Any(Function(x) x = 0)  ' Noncompliant
        goodList.GetList()?.GetList()?.GetList().GetList().Any(Function(x) x = 0)   ' Noncompliant
        goodList.GetList()?.GetList()?.GetList().GetList()?.Any(Function(x) x = 0)  ' Noncompliant
        goodList.GetList()?.GetList()?.GetList()?.GetList().Any(Function(x) x = 0)  ' Noncompliant
        goodList.GetList()?.GetList()?.GetList()?.GetList()?.Any(Function(x) x = 0) ' Noncompliant
    End Sub

    Private Sub CheckDelegate(ByVal intList As List(Of Integer), ByVal stringList As List(Of String), ByVal refList As List(Of ClassA), ByVal intArray As Integer(), ByVal someString As String, ByVal someInt As Integer, ByVal anotherInt As Integer, ByVal someRef As ClassA)
        intList.Any(Function(x) x = 0) ' Noncompliant
        intList.Any(Function(x) 0 = x) ' Noncompliant
        intList.Any(Function(x) x = someInt) ' Noncompliant
        intList.Any(Function(x) someInt = x) ' Noncompliant
        intList.Any(Function(x) x.Equals(0)) ' Noncompliant
        intList.Any(Function(x) 0.Equals(x)) ' Noncompliant

        intList.Any(Function(x) x = x) ' Compliant
        intList.Any(Function(x) someInt = anotherInt) ' Compliant
        intList.Any(Function(x) someInt = 0) ' Compliant
        intList.Any(Function(x) 0 = 0) ' Compliant

        intList.Any(Function(x) x.Equals(x)) ' Compliant
        intList.Any(Function(x) someInt.Equals(anotherInt)) ' Compliant
        intList.Any(Function(x) someInt.Equals(0)) ' Compliant
        intList.Any(Function(x) 0.Equals(0)) ' Compliant
        intList.Any(Function(x) x.Equals(x + 1)) ' Compliant

        intList.Any(Function(x) x.GetType() Is GetType(Integer)) ' Compliant
        intList.Any(Function(x) x.GetType().Equals(GetType(Integer))) ' Compliant
        intList.Any(Function(x) MyIntCheck(x)) ' Compliant
        intList.Any(Function(x) x <> 0)     ' Compliant
        intList.Any(Function(x) x.Equals(0) AndAlso True)   ' Compliant
        intList.Any(Function(x) If(x = 0, 2, 0) = 0) ' Compliant

        stringList.Any(Function(x) Equals(x, "")) ' Noncompliant
        stringList.Any(Function(x) Equals("", x)) ' Noncompliant
        stringList.Any(Function(x) Equals(x, someString)) ' Noncompliant
        stringList.Any(Function(x) Equals(someString, x)) ' Noncompliant
        stringList.Any(Function(x) x.Equals("")) ' Noncompliant
        stringList.Any(Function(x) "".Equals(x)) ' Noncompliant
        stringList.Any(Function(x) Equals(x, "")) ' Noncompliant

        stringList.Any(Function(x) MyStringCheck(x)) ' Compliant
        stringList.Any(Function(x) Not Equals(x, ""))     ' Compliant
        stringList.Any(Function(x) x.Equals("") AndAlso True)   ' Compliant
        stringList.Any(Function(x) Equals(If(Equals(x, ""), "a", "b"), "a")) ' Compliant
        stringList.Any(Function(x) x.Equals("" & someString)) ' Compliant

        intArray.Any(Function(x) x = 0) ' Compliant
        intArray.Any(Function(x) 0 = x) ' Compliant
        intArray.Any(Function(x) x = someInt) ' Compliant
        intArray.Any(Function(x) someInt = x) ' Compliant
        intArray.Any(Function(x) x.Equals(0)) ' Compliant
        intArray.Any(Function(x) 0.Equals(x)) ' Compliant
        intArray.Any(Function(x) someInt.Equals(x)) ' Compliant
        intArray.Any(Function(x) x.Equals(x + 1)) ' Compliant

        refList.Any(Function(x) x Is someRef) ' Compliant
        refList.Any(Function(x) someRef Is x) ' Compliant
        refList.Any(Function(x) x.Equals(someRef)) ' Noncompliant
        refList.Any(Function(x) someRef.Equals(x)) ' Noncompliant
        refList.Any(Function(x) Equals(someRef, x)) ' Noncompliant
        refList.Any(Function(x) Equals(x, someRef)) ' Noncompliant

        intList.Any(Function(x) x Is Nothing) ' Error [BC30020]
        intList.Any(Function(x) x.Equals(Nothing)) ' Noncompliant FP (warning: the result of this expression will always be false since a value-type is never equal to null)
        intList.Any(Function(x) Equals(x, Nothing)) ' Noncompliant FP (warning: the result of this expression will always be false since a value-type is never equal to null)

        refList.Any(Function(x) x = Nothing) ' Error [BC30452]
        refList.Any(Function(x) x.Equals(Nothing)) ' Noncompliant
        refList.Any(Function(x) Equals(x, Nothing)) ' Noncompliant
    End Sub

    Private Function MyIntCheck(ByVal x As Integer) As Boolean
        Return x = 0
    End Function
    Private Function MyStringCheck(ByVal x As String) As Boolean
        Return Equals(x, "")
    End Function

    Private Function Any(Of T)(ByVal predicate As Func(Of T, Boolean)) As Boolean
        Return True
    End Function

    Private Sub AcceptMethod(Of T)(ByVal methodThatLooksLikeAny As Func(Of Func(Of T, Boolean), Boolean))
    End Sub

    Friend Class GoodList(Of T)
        Inherits List(Of T)
        Public Function GetList() As GoodList(Of T)
            Return Me
        End Function
    End Class

    Friend Class EnumList(Of T)
        Implements IEnumerable(Of T)
        Public Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
            Return Nothing
        End Function
        Private Function GetEnumerator1() As IEnumerator Implements IEnumerable.GetEnumerator
            Return Nothing
        End Function
    End Class

    Friend Class ClassA
        Public myListField As List(Of Integer) = New List(Of Integer)()

        Public Property myListProperty As List(Of Integer)
            Get
                Return myListField
            End Get
            Set(ByVal value As List(Of Integer))
                myListField.AddRange(value)
                Dim b = myListField.Any(Function(x) x = 0) ' Noncompliant
                Dim c = myListField.Exists(Function(x) x = 0) ' Compliant
            End Set
        End Property

        Public classB As ClassB = New ClassB()
    End Class
End Class

Public Class ClassB
    Public myListField As List(Of Integer) = New List(Of Integer)()

    Public Function Any(ByVal predicate As Func(Of Integer, Boolean)) As Boolean
        Return False
    End Function

    Private Sub CheckEquals(ByVal intList As List(Of Integer), ByVal someInt As Integer)
        intList.Any(Function(x) Equals(x, someInt, someInt)) ' Compliant
    End Sub

    Private Function Equals(ByVal a As Integer, ByVal b As Integer, ByVal c As Integer) As Boolean
        Return False
    End Function
End Class
