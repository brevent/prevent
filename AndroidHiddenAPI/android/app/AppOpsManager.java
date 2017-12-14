package android.app;

/**
 * Created by thom on 2017/12/14.
 */

public class AppOpsManager {

    public static final int MODE_ALLOWED = 0;

    public static final int MODE_IGNORED = 1;

    /** @hide Control whether an application is allowed to run in the background. */
    public static int OP_RUN_IN_BACKGROUND = 63;
}
