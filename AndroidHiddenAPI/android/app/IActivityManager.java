package android.app;

import android.os.RemoteException;

public interface IActivityManager {

    public void forceStopPackage(final String packageName) throws RemoteException;

    public void forceStopPackage(final String packageName, int userId) throws RemoteException;

}
