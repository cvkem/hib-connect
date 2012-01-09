package vinzi.cdfdeMgt.domain;


public class TrackInfo {

    public Long id;
    public String track_name;
    public String file_location;

    public TrackInfo() {}

    public TrackInfo(String tn, String fl) {
	this.track_name = tn;
	this.file_location = fl;
	return;
    }
   public Long getId() { return id;}
   public void setId(Long id)  {this.id = id; }

    public String toString() {
	return "track_name = " + this.track_name + "\n" + "file_location = "+ this.file_location;
    }
   public String getTrack_name() { return this.track_name;}
   public void setTrack_name(String tn) {this.track_name = tn;}

   public String getFile_location() {return file_location;}
   public void setFile_location(String tn) {this.file_location = tn;}
}
