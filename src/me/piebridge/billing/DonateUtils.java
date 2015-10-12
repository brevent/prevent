package me.piebridge.billing;

import android.util.Base64;

import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.util.Collection;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/11.
 */
public class DonateUtils {

    public static final int REQUEST_CODE = 0x1000;

    public static final int API_VERSION = 3;

    public static final String ITEM_ID = "donate";

    public static final String ITEM_TYPE = "inapp";

    private static final String KEY = "" +
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzy78MWtWig+zBO06aNX4U8IV/rIDJVAB" +
            "lWAJxDnGy/I0R5OkJlyISwPLoSdVGFe7cSq3xR+i8hTtuhZQjtFiCORtUE3MVI++SHjMSbqU+Ya6" +
            "iqpUhFtankvU/vaozofB3pXvoPsYsPB/FAAD+Tou1LQ2x1X2+oX63X6Rwh9mNuxYxh7WIGT6bL71" +
            "waaBQmzf6KSVlL9kzCvJBJ1d/29olHRqnarozfrJW5RP0iUlSN9Ca0ZDoutvCCHxx6s63zh53FxJ" +
            "AtelMYQXma3/O7D/yXT1pXbwJOHTgXVjYZKADKxXgc6ptfoLPM5k5m1Jl03MI57F3cLMLj+X/1SD" +
            "D2KTKwIDAQAB";

    private static PublicKey publicKey = generatePublicKey();

    private DonateUtils() {

    }

    public static boolean isEmpty(Collection<String> collection) {
        return collection == null || collection.isEmpty();
    }

    public static boolean verify(String data, String signature) {
        Signature sig;
        try {
            sig = Signature.getInstance("SHA1withRSA");
            sig.initVerify(publicKey);
            sig.update(data.getBytes());
            return sig.verify(Base64.decode(signature, Base64.DEFAULT));
        } catch (GeneralSecurityException e) {
            UILog.e("security exception", e);
        }
        return false;
    }

    private static PublicKey generatePublicKey() {
        try {
            byte[] decodedKey = Base64.decode(KEY, Base64.DEFAULT);
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            return keyFactory.generatePublic(new X509EncodedKeySpec(decodedKey));
        } catch (GeneralSecurityException e) {
            UILog.e("cannot generate public key", e);
            return null;
        }
    }

}
