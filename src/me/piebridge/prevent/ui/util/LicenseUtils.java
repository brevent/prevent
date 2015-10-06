package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.os.SystemClock;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/5.
 */
public class LicenseUtils {

    private static final byte[] MODULUS = {
             -93, -117,  -85,   56,  -65,   -8,  -86,   59,   52,   43,  -50,  -47,   64,   51,   89, -116,
              95,  120,  -85,  -82,  -60,  -78,   65,   80,   18,   78, -109,   61,  106,  -28,  112,   76,
             124,   32,   94,   -4,  103,  -31,  -81,   17,  -15,   58,   -1, -120,  103,   49,  -64,   29,
              25,  107,  -16,   85, -126,   92,  -85,    0,  -54,  -45, -120,  -50,   -9,   69,  -81, -113,
            -108,    7, -101,   69,  -57,   31,  105, -114,   42,   55,  -26, -103,   48,   23,  -51,   16,
            -102,  102,  -52,   78,  123,   74,   64,  -88,   18,  111,   71,   66,   -1,    6,  109,   29,
             -17,   -2,  -30,   94,   -5,    0,   61,   21,   31,   47,  -97,  -12,   76,  -37,  -40,  -40,
              23,  -41,  -89,  -21,   48,   52,  -76,  -34,  106,   41, -119,  119, -113,   -9,  -44, -103,
              31,  -38,    9,   34,  125, -121,   61, -111,   25, -113,  120,  -74, -105,   76,  -86,  114,
              38,  -38,  100,  117,    1,   37,   97,   11,  -34,   79,  -87,  106,  -56,  -75,  -72,   12,
             -50, -116,  100,   83, -123,  104,   50, -109,   55,   25,  125,   49,   49,  -99,   94,  -37,
              95,   -3,  -62,   33,  -82,  125,  -29,  -60,   60,   70,  -30,  127,   91,   76,   -3,  120,
             -84,  -76,  -69,    1,  100,    0,   84,  -43,  110,  -21,  113, -115,   81,  -43,  -21,  -26,
            -124,   76,  -87,   59,  -82,   38,   46, -126,   33,    0,   96,  -20, -101,   17,   43, -121,
               6,  -37,  -13,  -99,  123,  -41,   69,  120,  111, -106,   31, -124,   91,   51,   89,  -96,
             126,   20,  -75,  108, -107,   16,  127,   56,  -36,  -17,  -24,  -92,  -34,  -48,   65,   73,
    };

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(0x1);

    private LicenseUtils() {

    }

    private static byte[] readKey(Context context) {
        byte[] key = new byte[0x100];
        for (File file : PreventListUtils.getExternalFilesDirs(context)) {
            if (file == null) {
                continue;
            }
            File path = new File(file, "license.key");
            if (path.isFile() && path.canRead()) {
                try {
                    InputStream is = new FileInputStream(path);
                    is.read(key);
                    is.close();
                    return key;
                } catch (IOException e) {
                    UILog.d("cannot get license", e);
                }
            }
        }
        return new byte[0];
    }

    public static String getLicense(final Context context) {
        long start = 0;
        if (!BuildConfig.RELEASE) {
            start = SystemClock.elapsedRealtime();
        }
        Future<String> future = executor.submit(new Callable<String>() {
            @Override
            public String call() throws Exception {
                return retrieveLicense(context);
            }
        });
        try {
            return future.get(0x1, TimeUnit.SECONDS);
        } catch (Throwable t) { // NOSONAR
            UILog.d("cannot get license", t);
            return null;
        } finally {
            if (!BuildConfig.RELEASE) {
                UILog.d("get license take " + (SystemClock.elapsedRealtime() - start) + "ms");
            }
        }
    }

    private static String retrieveLicense(Context context) {
        byte[] key = readKey(context);
        if (key.length == 0) {
            return null;
        }
        BigInteger exponent = BigInteger.valueOf(0x10001);
        BigInteger modulus = new BigInteger(1, MODULUS);
        byte[] signature = new BigInteger(1, key).modPow(exponent, modulus).toByteArray();
        int size = signature.length;
        for (int i = 0; i < size; ++i) {
            if (signature[i] == 0x00) {
                return new String(signature, i + 1, signature.length - i - 1);
            }
        }
        return null;
    }

}
