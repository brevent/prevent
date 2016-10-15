package com.android.internal.app;

public interface IAppOpsService {

    void setMode(int code, int uid, String packageName, int mode) throws android.os.RemoteException;

    class Stub {

        public static com.android.internal.app.IAppOpsService asInterface(android.os.IBinder binder) {
            throw new UnsupportedOperationException();
        }

    }

}
