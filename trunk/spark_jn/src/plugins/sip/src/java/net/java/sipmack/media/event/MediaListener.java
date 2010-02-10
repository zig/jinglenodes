/**
 * $Revision: $
 * $Date: $
 *
 * Copyright (C) 2009 Jive Software. All rights reserved.
 *
 * This software is published under the terms of the GNU Lesser Public License (LGPL),
 * a copy of which is included in this distribution.
 */
package net.java.sipmack.media.event;

public interface MediaListener extends java.util.EventListener {
    public void nonFatalMediaErrorOccurred(MediaErrorEvent evt);

    public void playerStarting(MediaEvent evt);

    public void playerStopped();
}