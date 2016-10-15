package android.app;

public interface INotificationManager {

    /* - api-24 */
    int getPackagePriority(String pkg, int uid) throws android.os.RemoteException;

    /* api-24 */
    int getPriority(String pkg, int uid) throws android.os.RemoteException;

    class Stub {

        public static android.app.INotificationManager asInterface(android.os.IBinder obj) {
            throw new UnsupportedOperationException();
        }

    }

}
