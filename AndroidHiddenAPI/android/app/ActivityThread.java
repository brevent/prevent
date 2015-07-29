package android.app;

import android.content.res.Configuration;
import android.os.IBinder;
import android.os.RemoteException;

/**
  * @hide
  */
public class ActivityThread {

    public ApplicationThread getApplicationThread() {
        return null;
    }


    private abstract class ApplicationThread implements IApplicationThread {
    }

}
