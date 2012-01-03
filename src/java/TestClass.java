package vinzi.test;

public class TestClass {
    public int val1;
    public int getVal1() { return val1;}
    public void setVal1(int val1)  {this.val1 = val1; };


    public String val2;
    public String getVal2() { return val2;}
    public void setVal2(String val2)  {this.val2 = val2; };


    public TestClass() {};

    public TestClass(int val1, String val2) {
	this.val1 = val1;
	this.val2 = val2;
	return;};

   public String toString() {
        return "val1=" + this.val1 + "val2=" + this.val2;};

};
