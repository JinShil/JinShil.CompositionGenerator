using JinShil.MixinGenerator;
using System;
using System.Collections.Generic;
using Xunit;

namespace JinShil.MixinGenerator.Test;

public partial class BasicTest
{
    partial class TestObject
    {
        [Mixin]
        readonly FieldsAndMethods _fieldsAndMethods = new();
    }

    class Fields
    {
        public string? FirstName { get; set; }
        public string? LastName { get; set; }

        public int Age { get; set; }
    }

    class FieldsAndMethods : Fields
    {
        public string? GetFullName()
        {
            return FirstName + " " + LastName;
        }

        public void SetFullName(string firstName, string lastName)
        {
            FirstName = firstName;
            LastName = lastName;
        }

        public void SetFullNameRef(ref string? firstName, ref string? lastName)
        {
            string? oldFirstName = FirstName;
            string? oldLastName = LastName;

            FirstName = firstName;
            LastName = lastName;

            firstName = oldFirstName;
            lastName = oldLastName;
        }

        public void VoidWithGenericParam(IEnumerable<string> e)
        { }

        public void VoidGeneric<T1, T2>(T1 arg, T2 arg2)
            where T2 : IEnumerable<T1>
            where T1 : class
        { }

        public T GenericReturn<T>(T arg)
        {
            return arg;
        }

        public void SetFullNameDefaults(string firstName = "FirstName", string lastName = "LastName")
        {
            FirstName = firstName;
            LastName = lastName;
        }

        public void SetAgeWithDefault(int age = 2)
        {
            Age = age;
        }

        public int GetAge()
        {
            return Age;
        }
    }

    [Fact]
    public void Test()
    {
        var o = new TestObject();

        // Tests forwarding of properties
        // o.FirstName = "FirstName";
        // o.LastName = "LastName";
        // Assert.Equal("FirstName LastName", o.GetFullName());

        // o.SetFullName("First", "Last");
        // Assert.Equal("First Last", o.GetFullName());

        // // Tests ref parameters
        // string? fName = "fName";
        // string? lName = "lName";
        // o.SetFullNameRef(ref fName, ref lName);
        // Assert.Equal("First", fName);
        // Assert.Equal("Last", lName);

        // o.VoidWithGenericParam(new string[] { "abc", "def" });

        // o.VoidGeneric<string, IEnumerable<string>>("abc", new string[] { "abc", "def" });

        // // Tests string default parameters
        // o.SetFullNameDefaults();
        // Assert.Equal("FirstName", o.FirstName);
        // Assert.Equal("LastName", o.LastName);
        // o.SetFullNameDefaults("FName", "LName");
        // Assert.Equal("FName", o.FirstName);
        // Assert.Equal("LName", o.LastName);

        // // Tests interger default parameter
        // o.Age = 0;
        // o.SetAgeWithDefault();
        // Assert.Equal(2, o.Age);
        // o.SetAgeWithDefault(3);
        // Assert.Equal(3, o.Age);

        // // Tests integer return from a method
        // o.Age = 5;
        // Assert.Equal(5, o.GetAge());

        // // Generic return
        // var gr = o.GenericReturn(1);
        // Assert.Equal(1, gr);
    }
}