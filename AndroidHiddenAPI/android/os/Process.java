package android.os;

public class Process {

    public static final int SYSTEM_UID = 1000;

    /**
      * @hide
      */
    public static final void readProcLines(String path, String[] reqFields, long[] outSizes) {
    }

    /**
      * @hide
      */
    public static final int getUidForPid(int pid) {
        return 0;
    }

    public static final void killProcess(int pid) {
    }

    public static final int myUid() {
        return 0;
    }

    public static final int myPid() {
        return 0;
    }

    public static final int myTid() {
        return 0;
    }

}
