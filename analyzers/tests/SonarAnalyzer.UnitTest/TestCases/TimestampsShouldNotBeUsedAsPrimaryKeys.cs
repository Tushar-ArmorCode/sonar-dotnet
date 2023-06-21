﻿using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DateTimeAlias = System.DateTime;
using KeyAttributeAlias = System.ComponentModel.DataAnnotations.KeyAttribute;

class TemporalTypes
{
    class Entity
    {
        public DateTime EntityId { get; set; }          // Noncompliant {{Timestamps should not be used as primary keys}}
        //     ^^^^^^^^
    }

    class DateTimeKey
    {
        public DateTime Id { get; set; }                // Noncompliant
    }

    class DateTimeOffsetKey
    {
        public DateTimeOffset Id { get; set; }          // Noncompliant
    }

    class TimeSpanKey
    {
        public TimeSpan Id { get; set; }                // Noncompliant
    }

    class DateTimeNoKey
    {
        public DateTime Identifier { get; set; }        // Compliant - only Id and [class-name]Id is recognized as key by Entity Framework
    }

    class DifferentCasing
    {
        public DateTime ID { get; set; }                // Noncompliant
    }

    class TemporalTypeWithFullName
    {
        public System.DateTime Id { get; set; }         // Noncompliant
    }

    class AliasedTemporalType
    {
        public DateTimeAlias Id { get; set; }           // FN - the chance of using a type aliased temporal type as a database key is slim, so we don't cover it to improve performance
    }
}

class NonTemporalTypes
{
    class IntKey
    {
        public int Id { get; set; }                     // Compliant - not a temporal type
    }

    class GuidKey
    {
        public Guid Id { get; set; }
    }

    class StringKey
    {
        public string Id { get; set; }
    }
}

class Attributes
{
    class SingleKeyAttribute
    {
        [Key]
        public DateTime KeyProperty { get; set; }       // Noncompliant
    }

    class KeyAndOtherAttributes
    {
        [Key, Column("KeyColumn")]
        public DateTime KeyProperty { get; set; }       // Noncompliant
    }

    class KeyAndOtherAttributeLists
    {
        [Column("KeyColumn")]
        [Key]
        public DateTime KeyProperty { get; set; }       // Noncompliant
    }

    class KeyWithAttributeName
    {
        [KeyAttribute]
        public DateTime KeyProperty { get; set; }       // Noncompliant
    }

    class KeyAttributeWithFullName
    {
        [System.ComponentModel.DataAnnotations.KeyAttribute]
        public DateTime KeyProperty { get; set; }       // Noncompliant
    }

    class AliasedKeyAttribute
    {
        [KeyAttributeAlias]
        public DateTime KeyProperty { get; set; }       // FN - we don't cover aliased attributes to improve the analzyer's performance
    }

    class NoKeyAttribute
    {
        [Column("KeyColumn")]
        public DateTime KeyProperty { get; set; }       // Compliant - not marked with Key attribute and not called Id or [ClassName]Id
    }

    class ForeignKeyRelationship
    {
        class Author
        {
            [Key]
            public DateTime DateOfBirth { get; set; }   // Noncompliant
            public ICollection<Book> Books { get; set; }
        }

        class Book
        {
            public string Title { get; set; }
            public Author Author { get; set; }
            [ForeignKey("Author")]
            public DateTime AuthorFK { get; set; }      // Compliant - only raise where the key is declared
        }
    }
}

class NotProperties
{
    public DateTime id;                                 // Compliant - only properties are validated
    public DateTime Id() => DateTime.Now;
}

class NonClassTypes
{
    struct StructEntity
    {
        public DateTime Id { get; set; }                // Compliant - struct types cannot be directly mapped to tables in Entity Framework
    }

    interface IEntity
    {
        DateTime Id { get; set; }                       // Compliant - issue will be raised in the implementing class
    }
}

class FluentApi
{
    class PersonDbContext: DbContext
    {
        public DbSet<Person> People { get; set; }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Person>()
                .HasKey(x => x.DateOfBirth);
        }
    }

    class Person
    {
        public DateTime DateOfBirth { get; set; }       // FN - keys created with the Fluent API are too complex to track
    }
}
