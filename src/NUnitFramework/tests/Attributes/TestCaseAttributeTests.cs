// ***********************************************************************
// Copyright (c) 2015 Charlie Poole
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// ***********************************************************************

using System;
using System.Collections;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.TestData.TestCaseAttributeFixture;
using NUnit.TestUtilities;
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework.Compatibility;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using System.Collections.Generic;
using NUnit.Framework.Internal.Builders;
using System.Reflection;

namespace NUnit.Framework.Attributes
{
    [TestFixture]
    public class TestCaseAttributeTests
    {
        [TestCaseDumb]
        [TestCaseDumb]
        [TestCaseDumb]
        public void DumbIntegerDivisionWithResultPassedToTest(int n, int d, int q)
        {
            Assert.AreEqual(q, 1);
            Assert.AreEqual(d, 1);
            Assert.AreEqual(n, 1);
        }

        [TestCase(12, 3, 4)]
        [TestCase(12, 2, 6)]
        [TestCase(12, 4, 3)]
        public void IntegerDivisionWithResultPassedToTest(int n, int d, int q)
        {
            Assert.AreEqual(q, n / d);
        }

        [TestCase(12, 3, ExpectedResult = 4)]
        [TestCase(12, 2, ExpectedResult = 6)]
        [TestCase(12, 4, ExpectedResult = 3)]
        public int IntegerDivisionWithResultCheckedByNUnit(int n, int d)
        {
            return n / d;
        }

        [TestCase(2, 2, ExpectedResult=4)]
        public double CanConvertIntToDouble(double x, double y)
        {
            return x + y;
        }

        [TestCase("2.2", "3.3", ExpectedResult = 5.5)]
        public decimal CanConvertStringToDecimal(decimal x, decimal y)
        {
            return x + y;
        }

        [TestCase(2.2, 3.3, ExpectedResult = 5.5)]
        public decimal CanConvertDoubleToDecimal(decimal x, decimal y)
        {
            return x + y;
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public decimal CanConvertIntToDecimal(decimal x, decimal y)
        {
            return x + y;
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public short CanConvertSmallIntsToShort(short x, short y)
        {
            return (short)(x + y);
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public byte CanConvertSmallIntsToByte(byte x, byte y)
        {
            return (byte)(x + y);
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public sbyte CanConvertSmallIntsToSByte(sbyte x, sbyte y)
        {
            return (sbyte)(x + y);
        }

        [TestCase("MethodCausesConversionOverflow", RunState.NotRunnable)]
        [TestCase("VoidTestCaseWithExpectedResult", RunState.NotRunnable)]
        [TestCase("TestCaseWithNullableReturnValueAndNullExpectedResult", RunState.Runnable)]
        public void TestCaseRunnableState(string methodName, RunState expectedState)
        {
            var test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), methodName).Tests[0];
            Assert.AreEqual(expectedState, test.RunState);
        }

        [TestCase("12-October-1942")]
        public void CanConvertStringToDateTime(DateTime dt)
        {
            Assert.AreEqual(1942, dt.Year);
        }

        [TestCase("4:44:15")]
        public void CanConvertStringToTimeSpan(TimeSpan ts)
        {
            Assert.AreEqual(4, ts.Hours);
            Assert.AreEqual(44, ts.Minutes);
            Assert.AreEqual(15, ts.Seconds);
        }

        [TestCase(null)]
        public void CanPassNullAsFirstArgument(object a)
        {
            Assert.IsNull(a);
        }

        [TestCase(new object[] { 1, "two", 3.0 })]
        [TestCase(new object[] { "zip" })]
        public void CanPassObjectArrayAsFirstArgument(object[] a)
        {
        }
  
        [TestCase(new object[] { "a", "b" })]
        public void CanPassArrayAsArgument(object[] array)
        {
            Assert.AreEqual("a", array[0]);
            Assert.AreEqual("b", array[1]);
        }

        [TestCase("a", "b")]
        public void ArgumentsAreCoalescedInObjectArray(object[] array)
        {
            Assert.AreEqual("a", array[0]);
            Assert.AreEqual("b", array[1]);
        }

        [TestCase(1, "b")]
        public void ArgumentsOfDifferentTypeAreCoalescedInObjectArray(object[] array)
        {
            Assert.AreEqual(1, array[0]);
            Assert.AreEqual("b", array[1]);
        }

        [TestCase(ExpectedResult = null)]
        public object ResultCanBeNull()
        {
            return null;
        }

        [TestCase("a", "b")]
        public void HandlesParamsArrayAsSoleArgument(params string[] array)
        {
            Assert.AreEqual("a", array[0]);
            Assert.AreEqual("b", array[1]);
        }

        [TestCase("a")]
        public void HandlesParamsArrayWithOneItemAsSoleArgument(params string[] array)
        {
            Assert.AreEqual("a", array[0]);
        }

        [TestCase("a", "b", "c", "d")]
        public void HandlesParamsArrayAsLastArgument(string s1, string s2, params object[] array)
        {
            Assert.AreEqual("a", s1);
            Assert.AreEqual("b", s2);
            Assert.AreEqual("c", array[0]);
            Assert.AreEqual("d", array[1]);
        }

        [TestCase("a", "b")]
        public void HandlesParamsArrayWithNoItemsAsLastArgument(string s1, string s2, params object[] array)
        {
            Assert.AreEqual("a", s1);
            Assert.AreEqual("b", s2);
            Assert.AreEqual(0, array.Length);
        }

        [TestCase("a", "b", "c")]
        public void HandlesParamsArrayWithOneItemAsLastArgument(string s1, string s2, params object[] array)
        {
            Assert.AreEqual("a", s1);
            Assert.AreEqual("b", s2);
            Assert.AreEqual("c", array[0]);
        }

        [TestCase("a", "b", Explicit = true)]
        public void ShouldNotRunAndShouldNotFailInConsoleRunner()
        {
            Assert.Fail();
        }

        [Test]
        public void CanSpecifyDescription()
        {
            Test test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodHasDescriptionSpecified").Tests[0];
            Assert.AreEqual("My Description", test.Properties.Get(PropertyNames.Description));
        }

        [Test]
        public void CanSpecifyTestName_FixedText()
        {
            Test test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodHasTestNameSpecified_FixedText").Tests[0];
            Assert.AreEqual("XYZ", test.Name);
            Assert.AreEqual("NUnit.TestData.TestCaseAttributeFixture.TestCaseAttributeFixture.XYZ", test.FullName);
        }

        [Test]
        public void CanSpecifyTestName_WithMethodName()
        {
            Test test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodHasTestNameSpecified_WithMethodName").Tests[0];
            var expectedName = "MethodHasTestNameSpecified_WithMethodName+XYZ";
            Assert.AreEqual(expectedName, test.Name);
            Assert.AreEqual("NUnit.TestData.TestCaseAttributeFixture.TestCaseAttributeFixture." + expectedName, test.FullName);
        }

        [Test]
        public void CanSpecifyCategory()
        {
            Test test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodHasSingleCategory").Tests[0];
            IList categories = test.Properties["Category"];
            Assert.AreEqual(new string[] { "XYZ" }, categories);
        }
 
        [Test]
        public void CanSpecifyMultipleCategories()
        {
            Test test = (Test)TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodHasMultipleCategories").Tests[0];
            IList categories = test.Properties["Category"];
            Assert.AreEqual(new string[] { "X", "Y", "Z" }, categories);
        }
 
        [Test]
        public void CanIgnoreIndividualTestCases()
        {
            TestSuite suite = TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodWithIgnoredTestCases");

            Test testCase = TestFinder.Find("MethodWithIgnoredTestCases(1)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Runnable));
 
            testCase = TestFinder.Find("MethodWithIgnoredTestCases(2)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Ignored));
 
            testCase = TestFinder.Find("MethodWithIgnoredTestCases(3)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Ignored));
            Assert.That(testCase.Properties.Get(PropertyNames.SkipReason), Is.EqualTo("Don't Run Me!"));
        }

        [Test]
        public void CanMarkIndividualTestCasesExplicit()
        {
            TestSuite suite = TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodWithExplicitTestCases");

            Test testCase = TestFinder.Find("MethodWithExplicitTestCases(1)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Runnable));
 
            testCase = TestFinder.Find("MethodWithExplicitTestCases(2)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Explicit));
 
            testCase = TestFinder.Find("MethodWithExplicitTestCases(3)", suite, false);
            Assert.That(testCase.RunState, Is.EqualTo(RunState.Explicit));
            Assert.That(testCase.Properties.Get(PropertyNames.SkipReason), Is.EqualTo("Connection failing"));
        }

#if !PORTABLE
        [Test]
        public void CanIncludePlatform()
        {
            bool isLinux = OSPlatform.CurrentPlatform.IsUnix;
            bool isMacOSX = OSPlatform.CurrentPlatform.IsMacOSX;
            
            TestSuite suite = TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodWithIncludePlatform");

            Test testCase1 = TestFinder.Find("MethodWithIncludePlatform(1)", suite, false);
            Test testCase2 = TestFinder.Find("MethodWithIncludePlatform(2)", suite, false);
            Test testCase3 = TestFinder.Find("MethodWithIncludePlatform(3)", suite, false);
            Test testCase4 = TestFinder.Find("MethodWithIncludePlatform(4)", suite, false);
            if (isLinux)
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Skipped));
            }
            else if (isMacOSX)
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Skipped));
            }
            else
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Skipped));
            }
        }

        [Test]
        public void CanExcludePlatform()
        {
            bool isLinux = OSPlatform.CurrentPlatform.IsUnix;
            bool isMacOSX = OSPlatform.CurrentPlatform.IsMacOSX;

            TestSuite suite = TestBuilder.MakeParameterizedMethodSuite(
                typeof(TestCaseAttributeFixture), "MethodWitExcludePlatform");

            Test testCase1 = TestFinder.Find("MethodWitExcludePlatform(1)", suite, false);
            Test testCase2 = TestFinder.Find("MethodWitExcludePlatform(2)", suite, false);
            Test testCase3 = TestFinder.Find("MethodWitExcludePlatform(3)", suite, false);
            Test testCase4 = TestFinder.Find("MethodWitExcludePlatform(4)", suite, false);
            if (isLinux)
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Runnable));
            }
            else if (isMacOSX)
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Runnable));
            }
            else
            {
                Assert.That(testCase1.RunState, Is.EqualTo(RunState.Skipped));
                Assert.That(testCase2.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase3.RunState, Is.EqualTo(RunState.Runnable));
                Assert.That(testCase4.RunState, Is.EqualTo(RunState.Runnable));
            }
        }
#endif


        #region Nullable<> tests

        [TestCase(12, 3, 4)]
        [TestCase(12, 2, 6)]
        [TestCase(12, 4, 3)]
        public void NullableIntegerDivisionWithResultPassedToTest(int? n, int? d, int? q)
        {
            Assert.AreEqual(q, n / d);
        }

        [TestCase(12, 3, ExpectedResult = 4)]
        [TestCase(12, 2, ExpectedResult = 6)]
        [TestCase(12, 4, ExpectedResult = 3)]
        public int? NullableIntegerDivisionWithResultCheckedByNUnit(int? n, int? d)
        {
            return n / d;
        }

        [TestCase(2, 2, ExpectedResult = 4)]
        public double? CanConvertIntToNullableDouble(double? x, double? y)
        {
            return x + y;
        }

        [TestCase(1)]
        public void CanConvertIntToNullableShort(short? x)
        {
            Assert.That(x.HasValue);
            Assert.That(x.Value, Is.EqualTo(1));
        }

        [TestCase(1)]
        public void CanConvertIntToNullableByte(byte? x)
        {
            Assert.That(x.HasValue);
            Assert.That(x.Value, Is.EqualTo(1));
        }

        [TestCase(1)]
        public void CanConvertIntToNullableSByte(sbyte? x)
        {
            Assert.That(x.HasValue);
            Assert.That(x.Value, Is.EqualTo(1));
        }

        [TestCase("2.2", "3.3", ExpectedResult = 5.5)]
        public decimal? CanConvertStringToNullableDecimal(decimal? x, decimal? y)
        {
            Assert.That(x.HasValue);
            Assert.That(y.HasValue);
            return x.Value + y.Value;
        }

        [TestCase(null)]
        public void SupportsNullableDecimal(decimal? x)
        {
            Assert.That(x.HasValue, Is.False);
        }

        [TestCase(2.2, 3.3, ExpectedResult = 5.5)]
        public decimal? CanConvertDoubleToNullableDecimal(decimal? x, decimal? y)
        {
            return x + y;
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public decimal? CanConvertIntToNullableDecimal(decimal? x, decimal? y)
        {
            return x + y;
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public short? CanConvertSmallIntsToNullableShort(short? x, short? y)
        {
            return (short)(x + y);
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public byte? CanConvertSmallIntsToNullableByte(byte? x, byte? y)
        {
            return (byte)(x + y);
        }

        [TestCase(5, 2, ExpectedResult = 7)]
        public sbyte? CanConvertSmallIntsToNullableSByte(sbyte? x, sbyte? y)
        {
            return (sbyte)(x + y);
        }

        [TestCase("12-October-1942")]
        public void CanConvertStringToNullableDateTime(DateTime? dt)
        {
            Assert.That(dt.HasValue);
            Assert.AreEqual(1942, dt.Value.Year);
        }

        [TestCase(null)]
        public void SupportsNullableDateTime(DateTime? dt)
        {
            Assert.That(dt.HasValue, Is.False);
        }

        [TestCase("4:44:15")]
        public void CanConvertStringToNullableTimeSpan(TimeSpan? ts)
        {
            Assert.That(ts.HasValue);
            Assert.AreEqual(4, ts.Value.Hours);
            Assert.AreEqual(44, ts.Value.Minutes);
            Assert.AreEqual(15, ts.Value.Seconds);
        }

        [TestCase(null)]
        public void SupportsNullableTimeSpan(TimeSpan? dt)
        {
            Assert.That(dt.HasValue, Is.False);
        }

        [TestCase(1)]
        public void NullableSimpleFormalParametersWithArgument(int? a)
        {
            Assert.AreEqual(1, a);
        }

        [TestCase(null)]
        public void NullableSimpleFormalParametersWithNullArgument(int? a)
        {
            Assert.IsNull(a);
        }

        [TestCase(null, ExpectedResult = null)]
        [TestCase(1, ExpectedResult = 1)]
        public int? TestCaseWithNullableReturnValue(int? a)
        {
            return a;
        }

        #endregion
    }

    /// <summary>
    /// TestCaseAttribute is used to mark parameterized test cases
    /// and provide them with their arguments.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true, Inherited = false)]
    public class TestCaseDumbAttribute : NUnitAttribute, ITestBuilder, ITestCaseData, IImplyFixture
    {
        #region Constructors

        /// <summary>
        /// Construct a TestCaseAttribute with a list of arguments.
        /// This constructor is not CLS-Compliant
        /// </summary>
        /// <param name="arguments"></param>
        public TestCaseDumbAttribute(params object[] arguments)
        {
            RunState = RunState.Runnable;

            if (arguments == null)
                Arguments = new object[] { null };
            else
                Arguments = arguments;

            Properties = new PropertyBag();
        }

        /// <summary>
        /// Construct a TestCaseAttribute with a single argument
        /// </summary>
        /// <param name="arg"></param>
        public TestCaseDumbAttribute(object arg)
        {
            RunState = RunState.Runnable;
            Arguments = new object[] { arg };
            Properties = new PropertyBag();
        }

        /// <summary>
        /// Construct a TestCaseAttribute with a two arguments
        /// </summary>
        /// <param name="arg1"></param>
        /// <param name="arg2"></param>
        public TestCaseDumbAttribute(object arg1, object arg2)
        {
            RunState = RunState.Runnable;
            Arguments = new object[] { arg1, arg2 };
            Properties = new PropertyBag();
        }

        /// <summary>
        /// Construct a TestCaseAttribute with a three arguments
        /// </summary>
        /// <param name="arg1"></param>
        /// <param name="arg2"></param>
        /// <param name="arg3"></param>
        public TestCaseDumbAttribute(object arg1, object arg2, object arg3)
        {
            RunState = RunState.Runnable;
            Arguments = new object[] { arg1, arg2, arg3 };
            Properties = new PropertyBag();
        }

        #endregion

        #region ITestData Members

        /// <summary>
        /// Gets or sets the name of the test.
        /// </summary>
        /// <value>The name of the test.</value>
        public string TestName { get; set; }

        /// <summary>
        /// Gets or sets the RunState of this test case.
        /// </summary>
        public RunState RunState { get; private set; }

        /// <summary>
        /// Gets the list of arguments to a test case
        /// </summary>
        public object[] Arguments { get; private set; }

        /// <summary>
        /// Gets the properties of the test case
        /// </summary>
        public IPropertyBag Properties { get; private set; }

        #endregion

        #region ITestCaseData Members

        /// <summary>
        /// Gets or sets the expected result.
        /// </summary>
        /// <value>The result.</value>
        public object ExpectedResult
        {
            get { return _expectedResult; }
            set
            {
                _expectedResult = value;
                HasExpectedResult = true;
            }
        }
        private object _expectedResult;

        /// <summary>
        /// Returns true if the expected result has been set
        /// </summary>
        public bool HasExpectedResult { get; private set; }

        #endregion

        #region Other Properties

        /// <summary>
        /// Gets or sets the description.
        /// </summary>
        /// <value>The description.</value>
        public string Description
        {
            get { return Properties.Get(PropertyNames.Description) as string; }
            set { Properties.Set(PropertyNames.Description, value); }
        }

        /// <summary>
        /// The author of this test
        /// </summary>
        public string Author
        {
            get { return Properties.Get(PropertyNames.Author) as string; }
            set { Properties.Set(PropertyNames.Author, value); }
        }

        /// <summary>
        /// The type that this test is testing
        /// </summary>
        public Type TestOf
        {
            get { return _testOf; }
            set
            {
                _testOf = value;
                Properties.Set(PropertyNames.TestOf, value.FullName);
            }
        }
        private Type _testOf;

        /// <summary>
        /// Gets or sets the reason for ignoring the test
        /// </summary>
        public string Ignore
        {
            get { return IgnoreReason; }
            set { IgnoreReason = value; }
        }

        /// <summary>
        /// Gets or sets a value indicating whether this <see cref="NUnit.Framework.TestCaseAttribute"/> is explicit.
        /// </summary>
        /// <value>
        /// <c>true</c> if explicit; otherwise, <c>false</c>.
        /// </value>
        public bool Explicit
        {
            get { return RunState == RunState.Explicit; }
            set { RunState = value ? RunState.Explicit : RunState.Runnable; }
        }

        /// <summary>
        /// Gets or sets the reason for not running the test.
        /// </summary>
        /// <value>The reason.</value>
        public string Reason
        {
            get { return Properties.Get(PropertyNames.SkipReason) as string; }
            set { Properties.Set(PropertyNames.SkipReason, value); }
        }

        /// <summary>
        /// Gets or sets the ignore reason. When set to a non-null
        /// non-empty value, the test is marked as ignored.
        /// </summary>
        /// <value>The ignore reason.</value>
        public string IgnoreReason
        {
            get { return Reason; }
            set
            {
                RunState = RunState.Ignored;
                Reason = value;
            }
        }

#if !PORTABLE
        /// <summary>
        /// Comma-delimited list of platforms to run the test for
        /// </summary>
        public string IncludePlatform { get; set; }

        /// <summary>
        /// Comma-delimited list of platforms to not run the test for
        /// </summary>
        public string ExcludePlatform { get; set; }
#endif

        /// <summary>
        /// Gets and sets the category for this test case.
        /// May be a comma-separated list of categories.
        /// </summary>
        public string Category
        {
            get { return Properties.Get(PropertyNames.Category) as string; }
            set
            {
                foreach (string cat in value.Split(new char[] { ',' }))
                    Properties.Add(PropertyNames.Category, cat);
            }
        }

        #endregion

        #region Helper Methods

        private TestCaseParameters GetParametersForTestCase(IMethodInfo method)
        {
            TestCaseParameters parms;

            try
            {
#if NETCF
                var tmethod = method.MakeGenericMethodEx(Arguments);
                if (tmethod == null)
                    throw new NotSupportedException("Cannot determine generic types from probing");
                method = tmethod;
#endif

                IParameterInfo[] parameters = method.GetParameters();
                int argsNeeded = parameters.Length;
                int argsProvided = Arguments.Length;

                parms = new TestCaseParameters(this);

                parms.Arguments = new object[] { 1, 1, 1 };

                if (argsProvided == argsNeeded)
                    PerformSpecialConversions(parms.Arguments, parameters);
            }
            catch (Exception ex)
            {
                parms = new TestCaseParameters(ex);
            }

            return parms;
        }

        /// <summary>
        /// Performs several special conversions allowed by NUnit in order to
        /// permit arguments with types that cannot be used in the constructor
        /// of an Attribute such as TestCaseAttribute or to simplify their use.
        /// </summary>
        /// <param name="arglist">The arguments to be converted</param>
        /// <param name="parameters">The ParameterInfo array for the method</param>
        private static void PerformSpecialConversions(object[] arglist, IParameterInfo[] parameters)
        {
            for (int i = 0; i < arglist.Length; i++)
            {
                object arg = arglist[i];
                Type targetType = parameters[i].ParameterType;

                if (arg == null)
                    continue;

                if (arg is SpecialValue && (SpecialValue)arg == SpecialValue.Null)
                {
                    arglist[i] = null;
                    continue;
                }

                if (targetType.IsAssignableFrom(arg.GetType()))
                    continue;
#if !PORTABLE
                if (arg is DBNull)
                {
                    arglist[i] = null;
                    continue;
                }
#endif
                bool convert = false;

                if (targetType == typeof(short) || targetType == typeof(byte) || targetType == typeof(sbyte) ||
                    targetType == typeof(short?) || targetType == typeof(byte?) || targetType == typeof(sbyte?) || targetType == typeof(double?))
                {
                    convert = arg is int;
                }
                else if (targetType == typeof(decimal) || targetType == typeof(decimal?))
                {
                    convert = arg is double || arg is string || arg is int;
                }
                else if (targetType == typeof(DateTime) || targetType == typeof(DateTime?))
                {
                    convert = arg is string;
                }

                if (convert)
                {
                    Type convertTo = targetType.GetTypeInfo().IsGenericType && targetType.GetGenericTypeDefinition() == typeof(Nullable<>) ?
                        targetType.GetGenericArguments()[0] : targetType;
                    arglist[i] = Convert.ChangeType(arg, convertTo, System.Globalization.CultureInfo.InvariantCulture);
                }
                else
                    // Convert.ChangeType doesn't work for TimeSpan from string
                    if ((targetType == typeof(TimeSpan) || targetType == typeof(TimeSpan?)) && arg is string)
                    {
                        arglist[i] = TimeSpan.Parse((string)arg);
                    }
            }
        }
        #endregion

        #region ITestBuilder Members

        /// <summary>
        /// Construct one or more TestMethods from a given MethodInfo,
        /// using available parameter data.
        /// </summary>
        /// <param name="method">The MethodInfo for which tests are to be constructed.</param>
        /// <param name="suite">The suite to which the tests will be added.</param>
        /// <returns>One or more TestMethods</returns>
        public IEnumerable<TestMethod> BuildFrom(IMethodInfo method, Test suite)
        {
            TestMethod test = new NUnitTestCaseBuilder().BuildTestMethod(method, suite, GetParametersForTestCase(method));

            yield return test;
        }

        #endregion
    }
}
