package vinzi.cdfdeMgt.util;
import org.hibernate.Session;

import java.util.*;
import vinzi.cdfdeMgt.domain.TrackRecord;
import vinzi.cdfdeMgt.domain.TrackInfo;
import vinzi.cdfdeMgt.util.HibernateUtil;

public class HibManager {

    public static void main(String[] args) {
	HibManager mgr = new HibManager();
	if (args[0].equals("store")) {
//	    mgr.createAndStoreTrackRecord("My Event", new Date());
	    mgr.createAndStoreTrackInfo("New Record", "testing twice");
	} 
	HibernateUtil.getSessionFactory().close();
    }

    private void createAndStoreTrackInfo(String contents, String loc) {
        Session session = HibernateUtil.getSessionFactory().getCurrentSession();
        session.beginTransaction();
        TrackInfo tr = new TrackInfo();
        tr.track_name = contents;
        tr.file_location = loc;
        session.save(tr);
        session.getTransaction().commit();
    }
/*
    private void createAndStoreTrackRecord(String contents, Date theDate) {
	Session session = HibernateUtil.getSessionFactory().getCurrentSession();
	session.beginTransaction();
	TrackRecord tr = new TrackRecord();
	tr.contents = contents;
	tr.datetime = theDate;
	session.save(tr);
	session.getTransaction().commit();
    }
*/
}
