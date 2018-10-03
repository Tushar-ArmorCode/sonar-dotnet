﻿Imports System
Imports System.Collections.Generic
Imports System.ServiceModel

Namespace Tests.Diagnostics
    Public Class MyAttribute
        Inherits Attribute

        Public Property isOneWay As Boolean
    End Class

    <ServiceContract>
    Interface IMyService1
        <OperationContract>
        Function MyServiceMethod() As Integer

        <OperationContract(IsOneWay:=True)>
        Function MyServiceMethod2() As Integer ' Noncompliant {{This method can't return any values because it is marked as one-way operation.}}
'                                      ^^^^^^^

        <OperationContract(IsOneWay:=False)>
        Function MyServiceMethod3() As Integer

        <OperationContract(IsTerminating:=True)>
        <My(IsOneWay:=True)>
        Function MyServiceMethod4() As Integer

        <OperationContract(IsOneWay:="mistake")>
        Function MyServiceMethod5() As Integer

        <OperationContract(IsOneWay:=True, AsyncPattern:=True)>
        Function BeginMyServiceMethod6() As IAsyncResult
    End Interface
End Namespace
