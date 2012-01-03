package vinzi.cdfdeMgt.domain;

import java.util.Date;

public class TrackRecord {

    public Long id;
    public Date datetime;
    public String contents;

    public TrackRecord() {}
    public TrackRecord(Date datetime, String contents) {
	this.datetime = datetime;
	this.contents = contents;}
}
