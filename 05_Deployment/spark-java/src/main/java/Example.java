import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.spark.api.java.function.FilterFunction;
import org.apache.spark.api.java.function.MapFunction;
import org.apache.spark.sql.*;

import static org.apache.spark.sql.functions.avg;
import static org.apache.spark.sql.functions.col;
import static org.apache.spark.sql.functions.max;


public class Example {
	  public static void main(String[] args) throws AnalysisException {
		  SparkSession session = SparkSession.builder()
				  .appName("Spark shell")
				  .master("spark://*******:****")
				  .getOrCreate();
	   DataFrameReader dataFrameReader = session.read();
	       Dataset<Row> df = dataFrameReader.json("C:/Users/sergi/Desktop/TFM/TFM-2021/05_Deployment/Spark/iris.json");
	       df.show();
	  }
      
}

















