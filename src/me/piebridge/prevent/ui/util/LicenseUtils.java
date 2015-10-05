package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.Signature;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.interfaces.RSAPublicKey;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/5.
 */
public class LicenseUtils {

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
        return key;
    }

    private static RSAPublicKey getPublicKey(Context context) {
        PackageInfo pi;
        try {
            pi = context.getPackageManager().getPackageInfo(BuildConfig.APPLICATION_ID, PackageManager.GET_SIGNATURES);
        } catch (PackageManager.NameNotFoundException e) {
            UILog.d("cannot get certificate", e);
            return null;
        }
        for (Signature signature : pi.signatures) {
            try {
                final CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
                final ByteArrayInputStream bais = new ByteArrayInputStream(signature.toByteArray());
                final Certificate cert = certFactory.generateCertificate(bais);
                return (RSAPublicKey) cert.getPublicKey();
            } catch (CertificateException e) {
                UILog.d("cannot get certificate", e);
            }
        }
        return null;
    }

    public static String getLicense(Context context) {
        RSAPublicKey publicKey = getPublicKey(context);
        byte[] signature = new BigInteger(1, readKey(context)).modPow(publicKey.getPublicExponent(), publicKey.getModulus()).toByteArray();
        int size = signature.length;
        for (int i = 0; i < size; ++i) {
            if (signature[i] == 0x00) {
                return new String(signature, i + 1, signature.length - i - 1);
            }
        }
        return null;
    }

}
