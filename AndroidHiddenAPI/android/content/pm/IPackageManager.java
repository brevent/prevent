package android.content.pm;

import android.content.ComponentName;

public interface IPackageManager {

    boolean isProtectedBroadcast(String actionName);

    /* since api-16 */
    ServiceInfo getServiceInfo(ComponentName className, int flags, int userId);

    int getPackageUid(String packageName, int flags, int userId);

}
