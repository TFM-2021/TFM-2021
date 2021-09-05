


import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.spark.api.java.function.FilterFunction;
import org.apache.spark.api.java.function.MapFunction;
import org.apache.spark.sql.*;

import static org.apache.spark.sql.functions.avg;
import static org.apache.spark.sql.functions.col;
import static org.apache.spark.sql.functions.max;



public class TFM_query {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		SparkSession spark = SparkSession
			      .builder()
			      .appName("MYSQL")
			      .master("local[*]")
			      .getOrCreate();
		
		 runJdbcDatasetExample(spark);
		 
		 spark.stop();
       
		
			
		}
		
	private static void runJdbcDatasetExample(SparkSession spark) {
		Dataset<Row> jdbcDF = spark.read()
			      .format("jdbc")
			      .option("url", "jdbc:mysql://*********:****/TFM")
			      .option("dbtable", "incendios_clean_final")
			      .option("user", "******")
			      .option("password", "******")
			      .load();
		jdbcDF.show();
	}
	

}







